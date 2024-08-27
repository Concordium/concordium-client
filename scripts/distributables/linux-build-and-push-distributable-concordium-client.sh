#!/usr/bin/env bash

docker build -t concordium/linux-distributable-concordium-client \
       -f scripts/distributables/linux-distributable-concordium-client.Dockerfile \
       --build-arg GHC_VERSION=$GHC_VERSION \
       .

mkdir out

docker run -e GHC_VERSION \
       -v $(pwd)/out:/out \
       concordium/linux-distributable-concordium-client

# The concordium-client binary is copied to the static-libraries bucket.
# We could consider adding a `static-binaries` one instead, but the jenkins 
# script likely should not copy the binary directly to the distribution bucket
# since we have to be very careful about what version we publish.
aws s3 cp out/concordium-client $OUTFILE --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
