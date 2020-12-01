# OSX build procedure
## Setup environment
```bash
cd

# install command line tools
xcode-select --install

# install ghcup + ghc 8.8.4 + cabal 3.2.0.0
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source .zshrc

# install happy and alex
cabal install happy alex --install-method=copy

# install stack
curl -sSL https://get.haskellstack.org/ | sh
export PATH=$PATH:$HOME/.local/bin
mkdir ~/.stack
echo "system-ghc: true" > ~/.stack/config.yaml

# install cargo
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source $HOME/.cargo/env

# install brew and some tools for later
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
brew install autoconf automake libtool protobuf xz

```

## Build our custom GHC
```
# clone ghc source
git clone -b ghc-8.8 https://gitlab.haskell.org/ghc/ghc.git --recurse-submodules
(
    cd ghc
    ./boot
    cat <<'EOF' > mk/build.mk
include mk/flavours/perf.mk
V=0

HADDOCK_DOCS = NO
BUILD_SPHINX_PDF = NO
INTEGER_LIBRARY = integer-simple

STRIP_CMD = :
EOF
    ./configure
    make -j5
    make binary-dist
    #ghc is built now in a file named ghc-8.8.4-x86_64-apple-darwin.tar.xz
    
    # install this ghc using ghcup
    ghcup install ghc -u "file:///$HOME/ghc/ghc-8.8.4-x86_64-apple-darwin.tar.xz" ghc-simple
    # remove the other ghc and set this one
    ghcup rm ghc 8.8.4 && ghcup set ghc ghc-simple
    # for some reason haddock is not even built with this script, perhaps because we set HADDOCK_DOCS = NO.
    cabal install haddock --install-method=copy
    ln -s ~/.cabal/bin/haddock ~/.ghcup/ghc/ghc-simple/bin/haddock
)
```

## Build the partially static binaries
```bash
# Clone and build our project
git clone git@gitlab.com:Concordium/consensus/simple-client --recurse-submodules
cd simple-client
nano deps/crypto/concordium-crypto.cabal # set the `static` flag to true
stack build --flag "scientific:integer-simple" --flag "cryptonite:-integer-gmp" --flag "integer-logarithms:-integer-gmp" --flag "hashable:-integer-gmp"
```

## Final binary
```bash
find `pwd`/.stack-work/install -type f -name "concordium-client" | xargs otool -L 
.stack-work/install/x86_64-osx/0ef06d07420f8754f5c7ad00371d13de56196d064e091f90862b54ccc0637e4f/8.8.4/bin/concordium-client:
	/usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1281.100.1)
	/usr/lib/libz.1.dylib (compatibility version 1.0.0, current version 1.2.11)
	/usr/lib/libiconv.2.dylib (compatibility version 7.0.0, current version 7.0.0)
	/usr/lib/libcharset.1.dylib (compatibility version 2.0.0, current version 2.0.0)
```
Distribute this binary - it only requires the basic system libraries, which OSX comes with.
## Notes
### Stack bugs
Due to a bug in [stack #5375](https://github.com/commercialhaskell/stack/issues/5375), which Javier has reported - it is needed to manually set the default value of the flag `static` to `True` in the `deps/crypto/concordium-crypto.cabal` file before running stack. When this bug has been fixed, or a better work around has been found this can be skipped.
