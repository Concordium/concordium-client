with (import <nixpkgs> { });

let
  moz_overlay = import (builtins.fetchTarball
    "https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz");
  nixpkgs = import ../nixpkgs { overlays = [ moz_overlay ]; };
  rustStableChannel =
    (nixpkgs.rustChannelOf { channel = "1.45.2"; }).rust.override {
      extensions =
        [ "rust-src" "rls-preview" "clippy-preview" "rustfmt-preview" ];
      targets = [ "x86_64-unknown-linux-musl" ];
    };
  ghc = nixpkgs.pkgsMusl.haskell.compiler.integer-simple.ghc883.override {
    enableRelocatedStaticLibs = true;
    enableShared = true;
    enableIntegerSimple = true;
    libffi = null;
  };

in with nixpkgs;
haskell.lib.buildStackProject {
  inherit ghc;
  name = "concordium_shell";
  hardeningDisable = [ "all" ];
  buildInputs = [
    rustStableChannel
    protobuf
    pkgconfig
    pkgsMusl.zlib.dev
    pkgsMusl.zlib.static
  ];
  PROTOC = "${pkgs.protobuf}/bin/protoc";
  CARGO_BUILD_TARGET = "x86_64-unknown-linux-musl";
}
