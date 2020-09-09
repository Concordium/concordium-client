#!/usr/bin/env bash

set -e

if [ ! -z "$JENKINS_HOME" ]; then
  if [[ $# -ge 1 ]]; then
    scripts/download_genesis_data.sh $1
    VERSION=$1
  else
    git clone git@gitlab.com:Concordium/genesis-data.git
    VERSION=`( cd genesis-data && git rev-parse --verify HEAD )`
    scripts/download_genesis_data.sh $VERSION
  fi
  cp genesis-data/*-bakers.tar.gz .
else
  VERSION=`( cd ../genesis-data && git rev-parse --verify HEAD )`
  scripts/download_genesis_data.sh $VERSION
  cp genesis-data/*-bakers.tar.gz .
fi

echo "Going to build a transaction generator which matches genesis @ $VERSION"

export DOCKER_BUILDKIT=1

docker build --ssh default -f scripts/Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/tx-generator:$VERSION .

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/tx-generator:$VERSION
