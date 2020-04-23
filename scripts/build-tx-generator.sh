#!/usr/bin/env bash
set -e

export STACK_ROOT="/build/.stack"

mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
./build-deps.sh
stack --version
./stack build --ghc-options -j4
ls /build/.stack-work/dist/x86_64-linux/