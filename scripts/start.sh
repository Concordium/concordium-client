#!/usr/bin/env bash

set -exo pipefail

# The application is only active if the node number mod this number is 0.
# The number of active generators is thus NUM_BAKERS/ID_MOD.
ID_MOD="${ID_MOD-10}" # Run tx-generator on every 10'th node by default.

if [[ -n "$BAKER_ID" && -n "$NUM_BAKERS" ]]; then
  ID=$(echo $BAKER_ID | cut -d'-' -f2)

  if [[ "$(($ID % $ID_MOD))" -eq 0 && -n "$TPS" && $TPS -gt 0 ]]; then
    ARGS="--tps $TPS"

    if [ -n "$LOGGING" ]; then
      ARGS="$ARGS --log"
    fi
    if [ -n "$ENCRYPTED" ]; then
      ARGS="$ARGS --encrypted"
    fi

    if [ -n "$FINBENCH_NUM" ]; then
      echo "Loading finbench num: $FINBENCH_NUM"
      tar -xzf /finbench-bakers.tar.gz -C /tmp
    elif [ -n "$TPS_NUM" ]; then
      echo "Loading TPS num: $TPS_NUM"
      tar -xzf /tps-bakers.tar.gz -C /tmp
    elif [ -n "$CATCHUP_NUM" ]; then
      echo "Loading catchup num: $CATCHUP_NUM"
      tar -xzf /catchup-bakers.tar.gz -C /tmp
    else
      echo "Loading num bakers: $NUM_BAKERS"
      tar -xzf /$NUM_BAKERS-bakers.tar.gz -C /tmp
    fi
    # Extract baker account for the ID of the companion node.
    mv /tmp/genesis_data/baker-account-$ID.json /tx_generator_account.json
    rm -rf tmp/genesis_data

    # Extract all account addresses as JSON (only strictly required for encrypted transfers).
    jq '[.[].address]' /non-baker-accounts.json > /accounts.json
    echo "Extracted accounts:"
    cat /accounts.json
    
    if [ -n "$ARTIFICIAL_START_DELAY" ]; then
      echo "Sleeping for $ARTIFICIAL_START_DELAY second(s) before starting generator so network can settle"
      sleep $ARTIFICIAL_START_DELAY
      echo "Starting..."
    fi

    /tx-generator --grpc-ip $GRPC_IP --grpc-port $GRPC_PORT --keyPair /tx_generator_account.json --addresses /accounts.json $ARGS
  else
    echo "Not running generator instance"
    while true; do sleep 5m; done
  fi
  
else
  echo "Not running transactions generator due to missing BAKER_ID and NUM_BAKERS"
  while true; do sleep 5m; done
fi
