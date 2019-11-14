#!/usr/bin/env bash
set -e

mkdir -p -m 0600 ~/.ssh && ssh-keyscan gitlab.com >> ~/.ssh/known_hosts
./build-deps.sh
stack --version
./stack build