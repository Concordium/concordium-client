#!/usr/bin/env bash
set -xeuo pipefail

readonly version=${1:?"Please provide a version number (e.g. '1.0.2')"}
year="$(date +"%Y")" # Used for copyright notices.
readonly year

readonly teamId="K762RM4LQ3"
readonly developerIdApplication="Developer ID Application: Concordium Software Aps ($teamId)"
readonly developerIdInstaller="Developer ID Installer: Concordium Software Aps ($teamId)"

# Get the location of this script.
macPackageDir="$(cd "$(dirname "${BASH_SOURCE[0]}")" &>/dev/null && pwd)"
readonly macPackageDir

readonly clientDir="$macPackageDir/../../../"

readonly buildDir="$macPackageDir/build"
readonly payloadDir="$buildDir/payload"

readonly pkgFile="$buildDir/packages/concordium-node.pkg"
readonly productFile="$buildDir/packages/concordium-node-$version-unsigned.pkg"
readonly signedProductFile="$buildDir/packages/concordium-node-$version.pkg"

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
    mkdir -p "$buildDir"
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

function signBinaries() {
    logInfo "Signing binaries..."

    # Find and sign all the binaries and dylibs.
    find "$payloadDir/Library" \
        -type f \
        -execdir sudo codesign -f --entitlement "$buildDir/entitlements.plist" --options runtime -s "$developerIdApplication" {} \;

    # Sign the installer plugin.
    sudo codesign -f --options runtime -s "$developerIdApplication" \
        "$buildDir/plugins/NodeConfigurationInstallerPlugin.bundle"

    logInfo "Done"
}

# Build the package.
# Look in the README.md for descriptions of the different files.
# The install-location is where to put the contents of the build/payload folder.
function buildPackage() {
    logInfo "Building package..."
    mkdir -p "$buildDir/packages"
    pkgbuild --identifier software.concordium.node \
        --version "$version" \
        --scripts "$buildDir/scripts" \
        --component-plist "$buildDir/components.plist" \
        --install-location "/" \
        --root "$payloadDir" \
        "$pkgFile"
    logInfo "Done"
}

# Sign the product.
function signProduct() {
    logInfo "Signing product..."
    productSign --sign "$developerIdInstaller" "$productFile" "$signedProductFile"
    logInfo "Done"
}

# Notarize the product and wait for it to finish.
# If successful, a notarization 'ticket' will be created on Apple's servers for the product.
# To enable offline installation without warnings, the ticket should be stapled onto the installer.
function notarize() {
    logInfo "Notarizing..."
    xcrun notarytool submit \
        "$signedProductFile" \
        --apple-id "$APPLEID" \
        --password "$APPLEIDPASS" \
        --team-id "$teamId" \
        --wait
    logInfo "Done"
}

# Staple the notarization ticket onto the installer.
function staple() {
    logInfo "Stapling..."
    xcrun stapler staple "$signedProductFile"
    logInfo "Done"
}

function signBuildAndNotarizeInstaller() {
    signBinaries
    buildPackage
    signProduct
    notarize
    logInfo "Build complete"
    logInfo "Installer located at:\n$signedProductFile"
}

function buildInstaller() {
    buildPackage
    logInfo "Build complete"
    logInfo "Installer located at:\n$productFile"
}

# Ask whether the installer should be signed or not.
# Then performs the selected action.
function promptToSignOrJustBuild() {
    while true; do
        read -rp "Do you wish to sign and notarize the installer [y/n]? " yn
        case $yn in
        [Yy]*)
            signBuildAndNotarizeInstaller
            break
            ;;
        [Nn]*)
            buildInstaller
            break
            ;;
        *) echo "Please answer yes or no." ;;
        esac
    done
}

function main() {
    printVersions
    cleanBuildDir
    createBuildDir
    compile
    copyCompiledItemsToBuildDir
    getDylibbundler
    collectDylibs
    pkgbuild --root ./payload --identifier com.Concordium.Software.Client --version "$version" --install-location / ConcordiumClient-"$version".pkg
    # promptToSignOrJustBuild
}

main
