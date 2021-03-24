#!/usr/bin/env sh

cd /build

sed -i "s/default: False/default: True/g" deps/concordium-base/package.yaml

stack build --stack-yaml stack.integer-simple.yaml

cp $(stack --stack-yaml stack.integer-simple.yaml path --local-install-root)/bin/concordium-client /out/concordium-client
