# OSX build procedure
## Setup environment
TBD

## Build the partially static binaries
```bash
$> stack build --flag concordium-crypto:forced-static-linking \
	--force-dirty
```
## Final binary
```bash
$> otool -L .stack-work/install/x86_64-osx/*/8.8.3/bin/concordium-client
.stack-work/install/x86_64-osx/0ef06d07420f8754f5c7ad00371d13de56196d064e091f90862b54ccc0637e4f/8.8.3/bin/concordium-client:
	/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1281.100.1)
	/usr/lib/libz.1.dylib (compatibility version 1.0.0, current version 1.2.11)
	/usr/lib/libiconv.2.dylib (compatibility version 7.0.0, current version 7.0.0)
	/usr/lib/libcharset.1.dylib (compatibility version 2.0.0, current version 2.0.0)
```
Distribute this binary - it only requires the basic system libraries, which OSX comes with.
## Notes
### Stack bugs
Due to a bug in [stack #5375](https://github.com/commercialhaskell/stack/issues/5375), which Javier has reported - it is needed to manually set the default value of the flag `forced-static-linking` to `True` in the `deps/crypto/concordium-crypto.cabal` file before running stack. When this bug has been fixed, or a better work around has been found this can be skipped.
