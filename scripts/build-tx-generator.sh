#!/usr/bin/env bash

apt-get update && apt-get install -y curl protobuf-compiler

export PATH=$PATH:$HOME/.cargo/bin:$HOME/.local/bin

curl -sSL https://get.haskellstack.org/ | sh

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

rustup default 1.37.0

./build-deps.sh

stack --version

./stack build
