# OSX build procedure
## Setup environment
Install latest 64bit MSYS using the [installer](http://repo.msys2.org/distrib/x86_64/msys2-x86_64-20200720.exe)
Start up `MSYS2 MinGW 64-bit` from the start menu under `MSYS2 64bit`
```bash
$> pacman -Syuu
$> pacman -S --needed git tar bsdtar binutils autoconf make xz \
    curl libtool automake python python3 p7zip patch ca-certificates \
    mingw-w64-$(uname -m)-gcc \
    mingw-w64-$(uname -m)-python3 \
    mingw-w64-$(uname -m)-python3-sphinx \
    mingw-w64-$(uname -m)-tools-git
$> curl -L https://downloads.haskell.org/~ghc/8.6.5/ghc-8.6.5-x86_64-unknown-mingw32.tar.xz | tar -xJ -C /mingw64 --strip-components=1
$> mkdir -p /usr/local/bin
$> curl -L https://downloads.haskell.org/cabal/cabal-install-3.0.0.0/cabal-install-3.0.0.0-x86_64-unknown-mingw32.zip | bsdtar -xzf- -C /usr/local/bin
$> cabal update
$> cabal install -j --installdir=/usr/local/bin --install-method=copy alex happy hscolour
$> cd /tmp
$> wget https://downloads.haskell.org/~ghc/8.8.3/ghc-8.8.3-src.tar.xz
$> tar xJf ghc-8.8.3-src.tar.xv
$> cd ghc-8.8.3
$> cp mk/build.mk.sample mk/build.mk
$> echo "INTEGER_LIBRARY=integer-simple" >> mk/build.mk
$> echo "BUILD_SPHINX_PDF=NO" >> mk/build.mk
$> echo "SRC_HC_OPTS += -fPIC" >> mk/build.mk
$> echo "SRC_CC_OPTS += -fPIC" >> mk/build.mk
$> ./configure --enable-tarballs-autodownload
$> make
$> make binary-dist
```

## Build the partially static binaries
We need to patch the `stack.yaml` file in order for it to pickup our new GHC binary distribution.
```bash
$> echo "setup-info: 
  ghc:  
    windows64-custom-integersimple: 
      8.8.3:  
        url: /tmp/ghc-8.8.3-FOO.tar.gz" >> stack.yaml
```
To build the binary do the following
```bash
$> stack build --flag \
    --flag hashable:-integer-gmp \
    --flag scientific:integer-simple \
    --flag integer-logarithms:-integer-gmp \
    --flag cryptonite:-integer-gmp \
    --system-ghc \
	--force-dirty
```
## Final binary
```bash
$> dir .stack-work/install/x86_64-osx/*/8.8.3/bin/concordium-client
.stack-work/install/x86_64-osx/0ef06d07420f8754f5c7ad00371d13de56196d064e091f90862b54ccc0637e4f/8.8.3/bin/concordium-client
```
Distribute this binary - it only requires the basic system libraries, which Windows comes with.
