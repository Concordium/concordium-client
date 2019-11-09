#!/usr/bin/env bash

apt-get update && apt-get install -y curl protobuf-compiler

export PATH=$PATH:$HOME/.cargo/bin:$HOME/.local/bin

curl -sSL https://get.haskellstack.org/ | sh

curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

rustup default 1.39.0

mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts

./build-deps.sh

stack --version

./stack build
