#!/usr/bin/env bash

#ID=$(printf "%016d\n" $(echo $BAKER_ID | cut -d'-' -f2))

#n=$((10#$ID % 2))

if [ "$BAKER_ID" == "node-0" ];
then
  echo "I am sending transactions!"
  /tx-generator --grpc-ip $GRPC_IP --grpc-port $GRPC_PORT
else
  while true; do sleep 5m; done
fi
