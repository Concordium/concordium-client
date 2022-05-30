#!/usr/bin/env sh

set -ex

cd /build

sed -i "s/default: False/default: True/g" deps/concordium-base/package.yaml

stack build --stack-yaml stack.linux-static.yaml

LOCAL_INSTALL_ROOT=$(stack --stack-yaml stack.linux-static.yaml path --local-install-root)
cp "$LOCAL_INSTALL_ROOT"/bin/concordium-client /out/concordium-client
