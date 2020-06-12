#!/usr/bin/env bash

# Script for wrapping the "stack run" command for invoking the client.
# The invoked stack command is printed for reference.
# Usage: ./run.sh NODE-ID ARGS...

# Extract arguments.
args=("$@")

function log {
    >&2 echo "$0: $*"
}

function docker_port {
    local -r node="$1"
    # Extract port of docker container running the node with the provided ID.
    local -r port="$(docker port "p2p-client_baker_$node" | grep '10000/tcp' | cut -d: -f2)"
    if [ -z "$port" ]; then
        log "Error: Cannot resolve docker port for node $node - falling back to port 0."
        echo 0
    else
        log "Resolved port $port for node $node."
        echo "$port"
    fi
}

if [ -z "$PORT" ]; then
    log "No port specified (via PORT) - resolving from docker."
    if [ -z "$NODE" ]; then
        log "No node specified (via NODE) - defaulting to 1."
        NODE=1
    fi
    PORT=$(docker_port "$NODE")
fi

# Build and run client.
# Do stack build && stack exec instead of stack run because it doesn't accept --flag nor --fast
# (and thus results in constant rebuilds).
log "> export LD_LIBRARY_PATH=deps/crypto/rust-src/target/release"
export LD_LIBRARY_PATH=deps/crypto/rust-src/target/release
(
    log "> stack build --profile --fast --flag 'simple-client:-middleware' --test --no-run-tests"
    stack build --profile --fast --flag 'simple-client:-middleware' --test --no-run-tests >&2
) && (
    log "> stack exec --profile concordium-client -- --grpc-ip 127.0.0.1 --grpc-port $PORT ${args[@]}"
    stack exec --profile concordium-client -- --grpc-ip 127.0.0.1 --grpc-port "$PORT" "${args[@]}"
)
