# Nix static build system

**NOTE: This build is not properly working at November 2020 with ghc884.**

The following guide has been tested on a NixOS unstable on mid August 2020. However it is also possible to use it with `nix` not on NixOS.
## Setup environment
This following below assumes that this repository is checked out in the directory named `concordium-client`
``` bash
$> nix-env -i stack # if already installed, this step can be skipped
$> cd ..
$> git clone https://github.com/NixOS/nixpkgs.git
$> cd nixpkgs
$> patch -p1 < ../concordium-client/scripts/nix-static-build/nixpkgs.patch
```
## Build the static binaries
```bash
$> stack --nix build --flag concordium-client:static \
	--flag hashable:-integer-gmp \
	--flag scientific:integer-simple \
	--flag integer-logarithms:-integer-gmp \
	--flag cryptonite:-integer-gmp \
  --flag concordium-crypto:link-with-musl
```
## Final binary
```bash
$> ldd .stack-work/install/x86_64-linux-nix/*/8.8.3/bin/concordium-client
	not a dynamic executable
```
Distribute this binary - it'll be fully static with libmusl and without GMP (ie no system GPL libraries linked)
## Notes
### Stack and nix
The shell file enabling this is configured in `stack.yaml` and is named `shell-stack.nix`. Due to the bug around [musl and ncurses](https://github.com/NixOS/nixpkgs/issues/85924) we need to use a locally cloned nixpkgs repo, which can be removed, and a straight import akin to `nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };`.

To force the nix-shell stack will be using to have the right version of `rustc` and the proper targets there's two important definitions
```  rustStableChannel =
    (nixpkgs.rustChannelOf { channel = "1.45.2"; }).rust.override {
      extensions =
        [ "rust-src" "rls-preview" "clippy-preview" "rustfmt-preview" ];
      targets = [ "x86_64-unknown-linux-musl" ];
    };
```
and
```
  CARGO_BUILD_TARGET = "x86_64-unknown-linux-musl";
```
This ensure that we'll only produce static (ie non `cdylib` outputs) and it'll be linked against libmusl without having to alter the `Cargo.toml` file in the crypto project.

Lastly it's important to configure the inherited `ghc` property and not use the one provided by stack when it launches the nix-shell - the following sniplet ensures that we allow for `-fPIC` when building static libraries, still allow for dynamically linked binaries, override `integer-simple` to be enabled, and force it to use the bundled `libffi`.
```
  ghc = nixpkgs.pkgsMusl.haskell.compiler.integer-simple.ghc883.override {
    enableRelocatedStaticLibs = true;
    enableShared = true;
    enableIntegerSimple = true;
    libffi = null;
  };
```

### Stack bugs
Due to a bug in [stack #5375](https://github.com/commercialhaskell/stack/issues/5375), which Javier has reported - it is needed to manually set the default value of the flag `link-with-musl` to `True` in the `deps/concordium-base/concordium-base.cabal` file before running stack. When this bug has been fixed, or a better work around has been found this can be skipped.
