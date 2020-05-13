#!/usr/bin/env bash

# Script for importing all test baker accounts from the p2p-client repo.

# Exit on error.
#set -e

genesis_dir="$1"
if [ -z "$genesis_dir" ]; then
    >&2 echo "Usage: $0 GENESIS_DIR"
    exit 0
elif ! [ -d "$genesis_dir" ]; then
    >&2 echo "Not a directory '$genesis_dir'"
    exit 1
fi

RUN="${RUN-./run.sh}"

if ! command -v $RUN >/dev/null; then
    >&2 echo "Run command '$RUN' not found."
    exit 2
fi

if ! command -v jq >/dev/null; then
    >&2 echo "This script requires the 'jq' tool to be on PATH."
    exit 2
fi

for f in "$genesis_dir"/baker-*-account.json; do
    name="$(basename "$f" | sed 's/baker-\(.*\)-.*/baker\1/')"
    $RUN config account import --name "$name" <(jq '{address: .address, accountKeys: .accountData.keys}' "$f")
done
