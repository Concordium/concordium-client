#!/usr/bin/env bash

set -e

if [[ -n "$BAKER_ID" && -n "$NUM_BAKERS" && -n "$MAX_AMOUNT_OF_GENERATORS" ]];
then
  ID=$(printf "%016d\n" $(echo $BAKER_ID | cut -d'-' -f2))

  if [[ "$ID" -lt "$MAX_AMOUNT_OF_GENERATORS" ]];
  then
    ARGS=""
    if [ -n "$BATCH_SIZE" ];
    then
      ARGS="$ARGS --batch $BATCH_SIZE"
    fi

    if [ -n "$DELAY" ]; 
    then
      ARGS="$ARGS --delay $DELAY"
    fi

    if [ -n "$START_NONCE" ]; 
    then
      ARGS="$ARGS --nonce $START_NONCE"
    fi

    tar xzf /$NUM_BAKERS-bakers.tar.gz -C /tmp
    mv /tmp/genesis_data/baker-$ID-account.json /tx_generator_account.json
    rm -rf /tmp/genesis_data
    /tx-generator --grpc-ip $GRPC_IP --grpc-port $GRPC_PORT --keyPair /tx_generator_account.json $ARGS
  else
    echo "Not running generator instance"
    while true; do sleep 5m; done
  fi
  
else
  echo "Not running transactions generator due to missing BAKER_ID, NUM_BAKERS, and MAX_AMOUNT_OF_GENERATORS"
  while true; do sleep 5m; done
fi