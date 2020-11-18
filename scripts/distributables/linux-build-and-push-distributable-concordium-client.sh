#!/usr/bin/env bash

docker build -t concordium/linux-distributable-concordium-client \
       -f scripts/distributables/linux-distributable-concordium-client.Dockerfile \
       --build-arg GHC_VERSION=$GHC_VERSION \
       --ssh default .

mkdir out

docker run -e GHC_VERSION \
       -v $(pwd)/out:/out \
       concordium/linux-distributable-concordium-client

aws s3 cp out/concordium-client s3://static-libraries.concordium.com/ --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
