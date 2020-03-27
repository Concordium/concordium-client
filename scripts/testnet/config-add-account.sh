#!/usr/bin/env bash

# Script for adding an account to the persisted configuration.

# Exit on error.
set -e

if ! command -v jq >/dev/null; then
    >&2 echo "This script requires the 'jq' tool to be on PATH."
    exit 2
fi

# Expect account name as argument.
name="$1"
if [ -z "$name" ]; then
    >&2 echo "Required argument 'name' is missing."
    exit 1
fi

# Expect web wallet style account export JSON on stdin.
payload="$(jq .)"
address="$(jq -r .address <<< "$payload")"
keys="$(jq .accountKeys <<< "$payload")"

base_cfg_dir="$HOME/.config/concordium"
acc_cfg_dir="$base_cfg_dir/accounts"
addr_cfg_dir="$acc_cfg_dir/$address"

mkdir -p "$addr_cfg_dir"

function write_file {
    local -r filename="$1"
    local -r contents="$2"

    >&2 echo "Writing '$filename'."
    echo "$contents" > "$filename"
}

function append_file {
    local -r filename="$1"
    local -r contents="$2"

    >&2 echo "Appending to '$filename'."
    echo "$contents" >> "$filename"
}

#echo "DEBUG: NAME=$name"
#echo "       ADDRESS=$address"

# Write account name map file.
append_file  "$acc_cfg_dir/names.map" "$name = $address"

# Write keys.

#echo "DEBUG: KEYS=$keys"

jq -r 'to_entries[] | .key + " " + .value.signKey + " " + .value.verifyKey' <<< "$keys" |
    while read line; do
        read idx sign_key verify_key <<< "$line"
        write_file "$addr_cfg_dir/$idx.sign" "$sign_key"
        write_file "$addr_cfg_dir/$idx.verify" "$verify_key"
    done
