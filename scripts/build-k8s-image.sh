#!/usr/bin/env bash

if [ ! -z "$JENKINS_HOME" ]; then
  git clone git@gitlab.com:Concordium/genesis-data.git
  if [[ $# -ge 1 ]]
  then
    ( cd genesis-data && git checkout $1 )
  fi
  cp genesis-data/*-bakers.tar.gz .
  VERSION=`( cd genesis && git rev-parse --verify HEAD )`
else
  cp ../genesis-data/*-bakers.tar.gz .
  VERSION=`( cd ../genesis-data && git rev-parse --verify HEAD )`
fi

echo "Going to build a transaction generator which matches genesis @ $VERSION"

export DOCKER_BUILDKIT=1

docker build --ssh default -f scripts/Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/tx-generator:$VERSION .
rm concordium.proto
rm *-bakers.tar.gz

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/tx-generator:$VERSION
