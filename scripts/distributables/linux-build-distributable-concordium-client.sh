#!/usr/bin/env sh

set -ex

cd /build

# Workaround for stack not setting flags for local dependencies properly.
sed -i "s/default: False/default: True/g" deps/concordium-base/package.yaml
sed -i "s/default: False/default: True/g" deps/concordium-base/concordium-base.cabal

stack build --stack-yaml stack.linux-static.yaml

LOCAL_INSTALL_ROOT=$(stack --stack-yaml stack.linux-static.yaml path --local-install-root)
mkdir -p /out
cp "$LOCAL_INSTALL_ROOT"/bin/concordium-client /out/concordium-client
