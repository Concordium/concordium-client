#!/usr/bin/env bash

# Script for wrapping the "stack run" command for invoking the client.
# The invoked stack command is printed for reference.
# Usage: ./run.sh NODE-ID ARGS...

# Extract arguments.
node="$1"
shift
args=("$@")

# Check that arguments were provided.
if [ "${#args}" -eq 0 ]; then
    >&2 echo "Error: no arguments provided"
    exit 1
fi

# Extract port of docker container running the node with the provided ID.
port="$(docker port "p2p-client_baker_$node" | cut -d: -f2)"
if [ -z "$port" ]; then
    >&2 echo "Error: cannot resolve docker port for node $node. Using port 0."
    port=0
fi

# Build and run client.
# Do stack build && stack exec instead of stack run because it doesn't accept --flag nor --fast
# (and thus results in constant rebuilds).
(
    >&2 echo "> LD_LIBRARY_PATH=deps/crypto/rust-src/target/release stack build --profile --fast --flag 'simple-client:-middleware' --test --no-run-tests"
    LD_LIBRARY_PATH=deps/crypto/rust-src/target/release stack build --profile --fast --flag 'simple-client:-middleware' --test --no-run-tests
) && (
    >&2 echo "> stack exec concordium-client -- --grpc-ip localhost --grpc-port $port ${args[@]}"
    stack exec concordium-client -- --grpc-ip localhost --grpc-port "$port" "${args[@]}"
)
