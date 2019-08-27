#!/usr/bin/env bash

if [ -z "$JENKINS_HOME" ]; then
  git clone git@gitlab.com:Concordium/p2p-client.git
  cat p2p-client/src/proto/concordium_p2p_rpc.proto > concordium.proto
else
  cat ../p2p-client/src/proto/concordium_p2p_rpc.proto > concordium.proto
fi

docker build -f scripts/Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/tx-generator .
rm concordium.proto

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/tx-generator

