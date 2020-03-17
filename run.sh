#!/usr/bin/env bash

# Script for wrapping the "stack run" command for invoking the client.
# The invoked stack command is printed for reference.
# Usage: ./run.sh NODE-ID ARGS...

# Extract arguments.
node="$1"
shift
args=("$@")

function fail {
	local -r msg="$1"
	local -r code="$2"

	>&2 echo "Error: $msg."
	exit "$code"
}

# Check that arguments were provided.
[ "${#args}" -eq 0 ] && fail "no arguments provided" 1

# Extract port of docker container running the node with the provided ID.
port="$(docker port "p2p-client_baker_$node" | cut -d: -f2)"
[ -z "$port" ] && fail "cannot resolve docker port for node $node" 2

# Construct command.
>&2 echo "> LD_LIBRARY_PATH=deps/crypto/rust-src/target/release stack run simple-client -- --grpc-ip localhost --grpc-port $port ${args[@]}"
LD_LIBRARY_PATH=deps/crypto/rust-src/target/release stack run simple-client -- --grpc-ip localhost --grpc-port "$port" "${args[@]}"
