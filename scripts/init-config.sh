#!/usr/bin/env bash

# Script for initializing the account configuration.
# The command expects the account address as parameter and the corresponding keys
# to be provided on stdin in the same JSON format as the one expected by the --keys flag.
# Note that because the script asks for confirmation, the keys cannot be piped into the command
# (unless they're prefixed by a newline).

# Exit on error.
set -e

# Read argument.
address="$1"

# Validate.
if [ -z "$address" ]; then
    >&2 echo "Required parameter 'address' is missing."
    exit 1
fi

if ! command -v jq >/dev/null; then
    >&2 echo "This script requires the 'jq' tool to be on PATH."
    exit 2
fi

base_cfg_dir="${XDG_CONFIG_HOME:-$HOME/.config}/concordium"
acc_cfg_dir="$base_cfg_dir/accounts"
addr_cfg_dir="$acc_cfg_dir/$address"

read -p "Press ENTER to confirm writing config to $base_cfg_dir (or Ctrl-C to cancel): "

rm -r "$base_cfg_dir"
mkdir -p "$addr_cfg_dir"

function write_file {
    local -r filename="$1"
    local -r contents="$2"

    echo "Writing '$filename'."
    echo "$contents" > "$filename"
}

# Write account name map file.
write_file  "$acc_cfg_dir/names.map" "default = $address"

# Write keys.
echo "Input keys in the same JSON format as the one expected by the --keys flag."

cat | jq -r 'to_entries[] | .key + " " + .value.signKey + " " + .value.verifyKey' |
    while read line; do
        read idx sign_key verify_key <<< "$line"
        write_file "$addr_cfg_dir/$idx.sign" "$sign_key"
        write_file "$addr_cfg_dir/$idx.verify" "$verify_key"
    done
