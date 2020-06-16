#!/usr/bin/env bash

set -e

if [[ -n "$BAKER_ID" && -n "$NUM_BAKERS" ]];
then
  ID=$(echo $BAKER_ID | cut -d'-' -f2)

  if [[ "$(($ID % 10))" -eq 0 && -n "$TPS" && $TPS -gt 0 ]];
  then
    ARGS="--tps $TPS"

    if [ -n "$LOGGING" ]; 
    then
      ARGS="$ARGS --log"
    fi

    if [ -n "$FINBENCH_NUM" ];
    then
      tar xzf /finbench-bakers.tar.gz -C /tmp
      mv /tmp/genesis_data/baker-$ID-account.json /tx_generator_account.json
      rm -rf /tmp/genesis_data
    elif [ -n "$TPS_NUM" ];
    then
      tar xzf /tps-bakers.tar.gz -C /tmp
      mv /tmp/genesis_data/baker-$ID-account.json /tx_generator_account.json
      rm -rf /tmp/genesis_data
    elif [ -n "$CATCHUP_NUM" ];
    then
      tar xzf /catchup-bakers.tar.gz -C /tmp
      mv /tmp/genesis_data/baker-$ID-account.json /tx_generator_account.json
      rm -rf /tmp/genesis_data
    else
      tar xzf /$NUM_BAKERS-bakers.tar.gz -C /tmp
      mv /tmp/genesis_data/baker-$ID-account.json /tx_generator_account.json
      rm -rf /tmp/genesis_data
    fi

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
  echo "Not running transactions generator due to missing BAKER_ID and NUM_BAKERS"
  while true; do sleep 5m; done
fi
