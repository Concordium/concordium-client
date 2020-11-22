#!/usr/bin/env sh

cd /build

sed -i "s/Default: False/Default: True/g" deps/crypto/concordium-crypto.cabal
sed -i 's/git-fetch-with-cli = true/git-fetch-with-cli = false/' deps/crypto/rust-src/.cargo/config

stack build --stack-yaml stack.integer-simple.yaml

cp $(stack --stack-yaml stack.integer-simple.yaml path --local-install-root)/bin/concordium-client /out/concordium-client
