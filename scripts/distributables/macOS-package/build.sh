#!/usr/bin/env bash
set -euo pipefail

# Print the usage message
function usage() {
    echo ""
    echo "Builds, signs and notarizes the installer package for the concordium client with a version number (e.g. '4.0.3')."
    echo ""
    echo "Usage: $0 [ --build VERSION ] [ --build-sign VERSION ] [ --sign PKGFILE VERSION ]"
    echo "  --build: Builds the client and its flat installer package."
    echo "  --build-sign: Builds, signs and notarizes the client and its flat installer package."
    echo "  --sign: Signs and notarizes the given installer package."
}

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
    --help)
        usage
        exit 0
        ;;
    --build)
        if [ -n "${BUILD-}" ] || [ -n "${SIGN-}" ]; then
            echo "ERROR: --build flag can not be used together with the other flags."
            usage
            exit 1
        fi
        if [ -z "${2-}" ]; then
            echo "ERROR: --build requires a version number as an argument."
            usage
            exit 1
        fi
        readonly version="$2"
        readonly BUILD=true
        shift
        ;;
    --build-sign)
        if [ -n "${BUILD-}" ] || [ -n "${SIGN-}" ]; then
            echo "ERROR: --build-sign flag can not be used together with the other flags."
            usage
            exit 1
        fi
        if [ -z "${2-}" ]; then
            echo "ERROR: --build-sign requires a version number as an argument."
            usage
            exit 1
        fi
        readonly version="$2"
        readonly BUILD=true
        readonly SIGN=true
        shift
        ;;
    --sign)
        if [ -n "${BUILD-}" ] || [ -n "${SIGN-}" ]; then
            echo "ERROR: --sign flag can not be used together with the other flags."
            usage
            exit 1
        fi
        if [ -z "${2-}" ]; then
            echo "ERROR: --sign requires a package file as an argument."
            usage
            exit 1
        fi
        if [ -z "${3-}" ]; then
            echo "ERROR: --sign requires a version number as an argument."
            usage
            exit 1
        fi
        pkgFile="${2-}"
        readonly version="$3"
        readonly SIGN=true
        shift
        shift
        ;;
    *)
        echo "Unknown option: $1"
        usage
        exit 1
        ;;
    esac
    shift
done

# At least one of 'sign' and 'build' arguments is required
if [ -z "${BUILD-}" ] && [ -z "${SIGN-}" ]; then
    echo "ERROR: You should provide either --build, --build-sign or --sign."
    usage
    exit 1
fi

readonly teamId="K762RM4LQ3"
readonly developerIdApplication="Developer ID Application: Concordium Software Aps ($teamId)"
readonly developerIdInstaller="Developer ID Installer: Concordium Software Aps ($teamId)"

# Get the location of this script.
macPackageDir="$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
readonly macPackageDir

readonly clientDir="$macPackageDir/../../../"

readonly buildDir="$macPackageDir/build"
readonly payloadDir="$buildDir/payload"
readonly binDir="$payloadDir/usr/local/bin"
readonly libDir="$payloadDir/usr/local/lib"

readonly pkgFile=${pkgFile-"$buildDir/concordium-client-$version-unsigned.pkg"}
readonly signedPkgFile="${pkgFile%.*}.pkg"

ghcVersion="$(stack --stack-yaml "$clientDir/stack.yaml" ghc -- --version | cut -d' ' -f8)" # Get the GHC version used.
readonly ghcVersion

if [ "$(arch)" == "arm64" ]; then
    readonly arch="aarch64"
else
    readonly arch="x86_64"
fi
readonly ghcVariant="$arch-osx-ghc-$ghcVersion"

# Log info in green color.
logInfo() {
    local GREEN='\033[0;32m'
    local NOCOLOR='\033[0m'
    printf "\n${GREEN}$@${NOCOLOR}\n"
}

function printVersions() {
    logInfo "Printing versions:"
    echo "stack version: $(stack --version)"
    echo "stack GHC version: $ghcVersion"
    echo "cargo version: $(cargo --version)"
    logInfo "Done"
}

function cleanBuildDir() {
    if [ -d "$buildDir" ]; then
        logInfo "Cleaning '$buildDir' folder"
        rm -rf "${buildDir:?}"
        logInfo "Done"
    fi
}

function createBuildDir() {
    logInfo "Creating build folder ..."
    mkdir -p "$binDir"
    mkdir -p "$libDir"
    logInfo "Done"
}

# Compile client.
function compile() {
    cd "$clientDir"
    logInfo "Building Client..."
    stack build --flag concordium-client:-middleware
    logInfo "Done"
}

# Copy the compiled binary to the build folder.
function copyCompiledItemsToBuildDir() {
    logInfo "Copy concordium-client to '$buildDir/"
    cp "$(stack path --local-install-root)/bin/concordium-client" "$binDir"
    logInfo "Done"
}

# Get the tool dylibbundler, which is used to recursively find and
# bundle the needed dylibs for a binary.
# Checks whether the tool is already installed and then potentially skips the build step.
function getDylibbundler() {
    logInfo "Getting dylibbundler..."
    if which dylibbundler >/dev/null; then
        logInfo "Done (skipped: already exists)"
    else
        brew install dylibbundler
        logInfo "Done"
    fi
}

# Use dylibbundler to recursively find and bundle the dylibs for node and collector.
# It moves all the dylibs into relative folder /libs and rewrites the binaries
# to look for the dylibs there.
# NB: This should be mostly reproducible across machines as we use a fixed resolver
# for Stack, and most of the dependencies are Haskell dylibs.
function collectDylibs() {

    function collectDylibsFor() {
        local fileToFix=${1:?"Missing file to fix with dylibbundler"}
        cd "$buildDir"
        # Paths to search for dylibs are added with the '-s' flag.
        dylibbundler --fix-file "$fileToFix" --bundle-deps --dest-dir "$libDir" --install-path "@executable_path/../lib/" --overwrite-dir \
            -s "$concordiumDylibDir" \
            -s "$stackSnapshotDir" \
            $stackLibDirs # Unquoted on purpose to use as arguments correctly
    }

    logInfo "Collecting dylibs with dylibbundler (this will take a few minutes)..."

    concordiumDylibDir=$(stack --stack-yaml "$clientDir/stack.yaml" path --local-install-root)"/lib/$ghcVariant"
    stackSnapshotDir=$(stack --stack-yaml "$clientDir/stack.yaml" path --snapshot-install-root)"/lib/$ghcVariant"
    # Use awk to preprend '-s ' to each dylib, to be used as argument for dylibbundler directly.
    stackLibDirs=$(find "$(stack --stack-yaml "$clientDir/stack.yaml" ghc -- --print-libdir)" -maxdepth 1 -type d | awk '{print "-s "$0}')
    readonly concordiumDylibDir
    readonly stackSnapshotDir
    readonly stackLibDirs

    logInfo " -- Processing concordium-client"
    collectDylibsFor "$binDir/concordium-client" &>/dev/null

    logInfo "Done"
}

# Extracts the installer package contents to the 'build' folder.
function expandInstallerPackage() {
    logInfo "Expanding package..."
    pkgutil --expand "$1" "$buildDir"
    cd "$buildDir"
    # Extract the payload content from the package.
    mv Payload Payload.gz
    gunzip Payload
    cpio -iv <Payload # creates a new folder 'usr'
    # Remove the redundant files.
    rm PackageInfo Bom Payload
    # Move the payload content to the 'payload' folder so that
    # it has the same structure as if it was built from scratch.
    mkdir "$payloadDir"
    mv usr "$payloadDir"
    logInfo "Done"
}

# Signs the binaries to be included in the installer with the developer application certificate.
function signBinaries() {
    logInfo "Signing binaries..."

    # Find and sign all the binaries and dylibs.
    find "$payloadDir" \
        -type f \
        -execdir sudo codesign -f --options runtime -s "$developerIdApplication" {} \;

    logInfo "Done"
}

# Signs the installer package with the developer installer certificate.
function signInstallerPackage() {
    logInfo "Signing installer package..."
    productSign --sign "$developerIdInstaller" "$pkgFile" "$signedPkgFile"
    logInfo "Done"
}

# Builds the installer package.
function buildInstallerPackage() {
    logInfo "Building package..."
    pkgbuild --identifier software.concordium.client \
        --version "$version" \
        --install-location / \
        --root "$payloadDir" \
        "$pkgFile"
    logInfo "Done"
}

# Notarizes the installer package and wait for it to finish.
# If successful, a notarization 'ticket' will be created on Apple's servers for the product.
# To enable offline installation without warnings, the ticket should be stapled onto the installer.
function notarize() {
    logInfo "Notarizing..."
    xcrun notarytool submit \
        "$signedPkgFile" \
        --apple-id "$APPLEID" \
        --password "$APPLEIDPASS" \
        --team-id "$teamId" \
        --wait
    logInfo "Done"
}

# Staple the notarization ticket onto the installer.
function staple() {
    logInfo "Stapling..."
    xcrun stapler staple "$signedPkgFile"
    logInfo "Done"
}

# Signs, builds and notarizes the installer package.
function signBuildAndNotarizeInstaller() {
    local tmpFile
    tmpFile="/tmp/concordium-client-$(date +%s).pkg"
    cp "$pkgFile" "$tmpFile"
    cleanBuildDir
    expandInstallerPackage "$tmpFile"
    rm "$tmpFile"
    signBinaries
    buildInstallerPackage
    signInstallerPackage
    notarize
    staple
    logInfo "Signing complete"
    logInfo "Signed installer located at:\n$signedPkgFile"
}

# Builds the concordium-client and creates the installer package.
function buildInstaller() {
    cleanBuildDir
    createBuildDir
    compile
    copyCompiledItemsToBuildDir
    getDylibbundler
    collectDylibs
    buildInstallerPackage
    logInfo "Build complete"
    logInfo "Installer located at:\n$pkgFile"
}

function main() {
    if [ -n "${BUILD-}" ]; then
        printVersions
        buildInstaller
    fi

    if [ -n "${SIGN-}" ]; then
        signBuildAndNotarizeInstaller
    fi
}

main
