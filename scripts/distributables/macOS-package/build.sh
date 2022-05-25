#!/usr/bin/env bash
set -xeuo pipefail

function usage() {
    echo "Builds, signs and notarizes the installer package for the concordium client."
    echo ""
    echo "Usage: $0 --version VERSION [--sign PKGFILE]"
    echo "  --version: Version number (e.g. '1.0.2')."
    echo "  --sign: Signs and notarizes the installer package without building."
}

while [[ $# -gt 0 ]]; do
    case $1 in
    --help)
        usage
        exit 0
        ;;
    --sign)
        if [ -z "${2-}" ]; then
            echo "ERROR: --sign requires a package file as an argument."
            exit 1
        fi
        pkgFile="$2"
        readonly SIGN=1
        shift
        ;;
    --version)
        readonly version="$2"
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

if [ -z "${version-}" ]; then
    echo "ERROR: --version is required."
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

readonly pkgFile="$buildDir/concordium-client.pkg"

ghcVersion="$(stack --stack-yaml "$clientDir/stack.yaml" ghc -- --version | cut -d' ' -f8)" # Get the GHC version used in Consensus.
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

# Create the 'build' folder from the 'template' folder.
# It copies the 'template' folder to 'build', creates a few new folders
# and replaces a number of variables in the files.
function createBuildDir() {
    logInfo "Creating build folder ..."
    mkdir -p "$payloadDir/usr/local/bin"
    mkdir -p "$payloadDir/usr/local/lib"
    logInfo "Done"
}

# Compile client.
function compile() {
    cd "$clientDir"
    logInfo "Building Client..."
    stack build --flag concordium-client:-middleware
    logInfo "Done"
}

# Copy the compiled items (binaries and supporting data) to the build folder.
function copyCompiledItemsToBuildDir() {
    logInfo "Copy concordium-client to '$buildDir/"
    cp "$(stack path --local-install-root)/bin/concordium-client" "$payloadDir/usr/local/bin/"
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
        dylibbundler --fix-file "$fileToFix" --bundle-deps --dest-dir "$payloadDir/usr/local/lib" --install-path "@executable_path/../lib/" --overwrite-dir \
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
    collectDylibsFor "$payloadDir/usr/local/bin/concordium-client" &>/dev/null
    # local fileToFix="$payloadDir/usr/local/bin/concordium-client"
    # cd "$buildDir"
    # # Paths to search for dylibs are added with the '-s' flag.
    # dylibbundler --fix-file "$fileToFix" --bundle-deps --dest-dir "./libs" --install-path "@executable_path/libs/" --overwrite-dir \
    #     -s "$concordiumDylibDir" \
    #     -s "$stackSnapshotDir" \
    #     $stackLibDirs # Unquoted on purpose to use as arguments correctly

    logInfo "Done"
}

# Extracts the installer package contents to the 'build' folder.
function expandInstallerPackage() {
    logInfo "Expanding package..."
    pkgutil --expand "$1" "$buildDir"
    cd "$buildDir"
    mv Payload Payload.gz
    gunzip Payload
    cpio -iv <Payload
    rm PackageInfo Bom Payload
    mkdir "$payloadDir"
    mv usr "$payloadDir"
    logInfo "Done"
}

# Signs the binaries to be included in the installer with the developer certificate.
function signBinaries() {
    logInfo "Signing binaries..."

    # Find and sign all the binaries and dylibs.
    find "$payloadDir" \
        -type f \
        -execdir sudo codesign -f --entitlement "$buildDir/entitlements.plist" --options runtime -s "$developerIdApplication" {} \;
    # -execdir sudo codesign -f --options runtime -s gdb_codesign {} \;

    logInfo "Done"
}

# Signs the installer package with the developer certificate.
function signInstallerPackage() {
    logInfo "Signing installer package..."

    # Find and sign all the binaries and dylibs.
    sudo codesign -f --entitlement "$buildDir/entitlements.plist" --options runtime -s "$developerIdInstaller" "$pkgFile"
    # sudo codesign -f --options runtime -s gdb_codesign "$pkgFile"

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
        "$pkgFile" \
        --apple-id "$APPLEID" \
        --password "$APPLEIDPASS" \
        --team-id "$teamId" \
        --wait
    logInfo "Done"
}

# Signs, builds and notarizes the installer package.
function signBuildAndNotarizeInstaller() {
    mv "$pkgFile" /tmp
    cleanBuildDir
    expandInstallerPackage /tmp/"$(basename "$pkgFile")"
    signBinaries
    buildInstallerPackage
    signInstallerPackage
    # notarize
    logInfo "Build complete"
    logInfo "Installer located at:\n$pkgFile"
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
    if [ -n "${SIGN-}" ]; then
        signBuildAndNotarizeInstaller
    else
        printVersions
        buildInstaller
        signBuildAndNotarizeInstaller
    fi
}

main
