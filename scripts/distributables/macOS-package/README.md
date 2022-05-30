# Concordium Client for macOS

## Build
Run the build script with a version number. For example:

``` sh
$ ./build 4.0.3
```

This produces an unsigned installer. The build script can also sign and notarize
the installer. See below.

## Sign and Notarize

For releases, the code and installer should be signed and the installer
notarized.

Signing requires a valid [Developer ID Application
Certificate](https://developer.apple.com/support/certificates/) and Developer ID
Installer Certificate in the Keychain of the machine used for building, and a valid Apple Developer ID has to be logged on the same machine.

For the notarizing process to succeed, the Apple ID and it's password also needs to be available as the following
environment variables:

-   APPLEID=<example@e-mail.com>
-   APPLEIDPASS=The password for the Apple ID

For the APPLEIDPASS, setting up an [app-specific password](https://support.apple.com/en-us/HT204397) for the Apple ID is recommended.

To sign and notarize the installer (.pkg) file from the build step, run:

``` sh
./build --sign <path-to-unsigned-pkg> --version <version-number>
```

If you are building and signing on the same computer (as opposed to building
with Jenkins), the whole process can be achieved with a single step:

``` sh
./build --build-sign <version>
```
