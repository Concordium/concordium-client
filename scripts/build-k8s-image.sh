#!/usr/bin/env bash

if [ ! -z "$JENKINS_HOME" ]; then
  git clone git@gitlab.com:Concordium/p2p-client.git
  if [[ $# -ge 1 ]]
  then
    ( cd p2p-client && git checkout $1 )
  fi
  cp p2p-client/scripts/genesis-data/*-bakers.tar.gz .
  VERSION=`( cd p2p-client && git rev-parse --verify HEAD )`
else
  cp ../p2p-client/scripts/genesis-data/*-bakers.tar.gz .
  VERSION=`( cd ../p2p-client && git rev-parse --verify HEAD )`
fi

echo "Going to build a transaction generator which matches p2p-client @ $VERSION"

export DOCKER_BUILDKIT=1

docker build --ssh default -f scripts/Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/tx-generator:$VERSION .
rm concordium.proto
rm *-bakers.tar.gz

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/tx-generator:$VERSION
