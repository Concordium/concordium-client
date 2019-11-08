#!/usr/bin/env bash

set -e

if [[ -n "$BAKER_ID" && -n "$NUM_BAKERS" && -n "$MAX_AMOUNT_OF_GENERATORS" ]];
then
  ID=$(echo $BAKER_ID | cut -d'-' -f2)

  if [[ "$ID" -lt "$MAX_AMOUNT_OF_GENERATORS" ]];
  then
    ARGS=""
    if [ -n "$TPS" ]; 
    then
      ARGS="$ARGS --tps $TPS"
    fi

    if [ -n "$LOGGING" ]; 
    then
      ARGS="$ARGS --log"
    fi

    tar xzf /$NUM_BAKERS-bakers.tar.gz -C /tmp
    mv /tmp/genesis_data/baker-$ID-account.json /tx_generator_account.json
    rm -rf /tmp/genesis_data

    if [ -n "$ARTIFICIAL_START_DELAY" ]; then
      echo "Sleeping for $ARTIFICIAL_START_DELAY second(s) before starting generator so network can settle"
      sleep $ARTIFICIAL_START_DELAY
    fi

    /tx-generator --grpc-ip $GRPC_IP --grpc-port $GRPC_PORT --keyPair /tx_generator_account.json $ARGS
  else
    echo "Not running generator instance"
    while true; do sleep 5m; done
  fi
  
else
  echo "Not running transactions generator due to missing BAKER_ID, NUM_BAKERS, and MAX_AMOUNT_OF_GENERATORS"
  while true; do sleep 5m; done
fi