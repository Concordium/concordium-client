#!/usr/bin/env bash

set -euxo pipefail

GENESIS_REF=$1

# Clone genesis-data and translate ref to SHA.
git clone --branch "$GENESIS_REF" --depth 1 git@gitlab.com:Concordium/genesis-data.git
GENESIS_SHA="$(cd genesis-data && git rev-parse --verify HEAD)"

# Extract 'non-baker-accounts.json' from repo and delete it to avoid name clash below.
cp genesis-data/non-baker-accounts.json .
rm -rf genesis-data

scripts/download-genesis-data.sh "$GENESIS_SHA"
cp genesis-data/*-bakers.tar.gz .
rm -rf genesis-data

echo "Going to build a transaction generator which matches genesis @ $GENESIS_SHA"

export DOCKER_BUILDKIT=1

docker build --ssh default -f scripts/Dockerfile -t 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/tx-generator:$GENESIS_SHA .

docker push 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/tx-generator:$GENESIS_SHA
