# Windows build procedure

The contents of this document is no longer used to build concordium-client.
However we leave it here since it contains information that is difficult to
find elsewhere, and if we ever need to have custom builds of ghc we will need to
do something similar to this.

## Build custom GHC

This is needed in order to link against `integer-simple` instead of `integer-gmp`.

### Setup environment

This section is based on [this guide](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/preparation/windows).

Install [MSYS2](http://repo.msys2.org/distrib/x86_64/). Make sure to start "MSYS2 MinGW" and not "MSYS2 MSYS" as
[GHC doesn't support that](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/preparation/windows)!
And make sure it’s the 64 bit version.
The link above mentions that the "regular" MSYS should be run right after installing to finish the setup.

Disabling anti-virus on the system (for the MinGW folders) could speed up things considerably.

Update system packages and install GHC dependencies:

```sh
pacman -Syuu # repeat until there are no more updates

pacman -S --needed git tar bsdtar binutils autoconf make xz zstd \
    curl libtool automake python python3 p7zip patch ca-certificates \
    mingw-w64-$(uname -m)-gcc \
    mingw-w64-$(uname -m)-python3 \
    mingw-w64-$(uname -m)-python3-sphinx \
    mingw-w64-$(uname -m)-tools-git
```

It's probably not a bad idea to restart after this.

Install GHC 8.8.4 and Cabal 3.2
(make sure to not use GHC 8.8.3 as it is [critically broken](https://gitlab.haskell.org/ghc/ghc/-/issues/17926)
on at least some Windows installations - see also [this](https://gitlab.haskell.org/ghc/ghc/-/issues/18496)):

```sh
curl -L https://downloads.haskell.org/~ghc/8.8.4/ghc-8.8.4-x86_64-unknown-mingw32.tar.xz | tar -xJ -C /mingw64 --strip-components=1 # "mingw32" is not a typo
mkdir -p /usr/local/bin
curl -L https://downloads.haskell.org/cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-mingw32.zip | bsdtar -xzf- -C /usr/local/bin
cabal update
```

Install dependencies of GHC:

```sh
cabal install -j --installdir=/usr/local/bin --install-method=copy alex happy hscolour
```

### Build

This section is based on [this guide](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/quick-start).

Clone and configure GHC source:

```sh
git clone --branch ghc-8.8 --recurse-submodules https://gitlab.haskell.org/ghc/ghc.git
cd ghc
cp mk/build.mk.sample mk/build.mk
echo "INTEGER_LIBRARY=integer-simple" >> mk/build.mk
echo "BUILD_SPHINX_PDF=NO" >> mk/build.mk
./boot
```

Build from the source:

```sh
./configure --enable-tarballs-autodownload
make -j8                                   # use 8 parallel jobs
make binary-dist
```

After a very long time, this results in the final GHC being written to the archive file
`ghc-8.8.4-x86\_64-unknown-mingw32.tar.xz`.
Move this file to root (`C:\mingw64` in Windows' FS) for `stack` to find it later:

```
mv ghc-8.8.4-x86\_64-unknown-mingw32.tar.xz /
```

## Build Concordium components

### Install build tools

#### Haskell

Install [Stack](https://get.haskellstack.org/stable/windows-x86_64-installer.exe) - set "Destination Folder"
`C:\msys64\mingw64\usr\local\bin` to avoid having to hack `PATH`.

#### Rust

The following components are written in Rust:

- `client-tool`
- `cargo-concordium`
- `node` (partially)
- `concordium-client` (indirectly)?

Install [Rustup](https://www.rust-lang.org/tools/install) to be able to compile those:
Customize installation:
- triple: `x64_64-pc-windows-gnu`
- toolchain: `1.62`

Add binaries to `PATH`:

```sh
echo 'PATH=/c/Users/$USER/.cargo/bin:$PATH' >>~/.bash_profile
```

Install correct toolchain if it wasn't done during install:

```sh
rustup default 1.62-x86_64-pc-windows-gnu
```

#### Protobuf

Some components depend on `protoc`; install:

```sh
pacman -S mingw-w64-x86_64-protobuf
```

Alternatively, copy [`proto.exe`](https://github.com/protocolbuffers/protobuf/releases/download/v3.14.0/protoc-3.14.0-win64.zip) into `C:\msys64\mingw64\bin`.

### Build concordium-client

Clone the source:

```sh
git clone  --recurse-submodules git@gitlab.com:Concordium/concordium-client
cd concordium-client
```

Configure `stack` to use custom GHC:

```sh
echo "setup-info: 
  ghc:  
    windows64-integersimple: 
      8.8.4:  
        url: C:\msys64\ghc-8.8.4-x86_64-unknown-mingw32.tar.xz
        
ghc-variant: integersimple" >> stack.yaml
```

Build the project:

```sh
stack build --flag "scientific:integer-simple" --flag "cryptonite:-integer-gmp" --flag "integer-logarithms:-integer-gmp" --flag "hashable:-integer-gmp" --force-dirty
```

This produces the final binary which should depend on basic system libraries only:

```sh
find $PWD/.stack-work/install -type f -name "concordium-client.exe"
# e.g. .../concordium-client/.stack-work/install/a42f9d36/bin/concordium-client.exe
```

TODO: should use `stack install`?

## Troubleshooting

### cygheap base mismatch detected

Stack may install a version of git which [confuses MSYS](https://stackoverflow.com/a/60525817/883073), resulting in:

```
0 [main] git (3212) C:\msys64\usr\lib\git-core\git.exe: *** fatal error - cygheap base mismatch detected - 0x180345408/0x18034B408.
```

The solution is to replace the DLL at path like
```
  C:\Users\$USER\AppData\Local\Programs\stack\x86_64-windows\msys2-20200903\usr\bin\msys-2.0.dll
```
by
```
  C:\msys64\usr\bin\msys-2.0.dll
```

TODO: should really be installing MSYS via
[stack](https://docs.haskellstack.org/en/stable/developing_on_windows/)?
