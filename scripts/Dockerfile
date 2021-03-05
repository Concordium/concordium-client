# syntax=docker/dockerfile:experimental
FROM 192549843005.dkr.ecr.eu-west-1.amazonaws.com/concordium/base:0.18 as build

COPY . /build
WORKDIR /build
RUN --mount=type=ssh ./scripts/build-tx-generator.sh

FROM ubuntu

# Install 'jq' to extract account addresses from genesis (passed to '--addresses').
RUN apt update && apt -y install jq

COPY --from=build /build/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.0.1.0/build/tx-generator/tx-generator /tx-generator
COPY --from=build /build/.stack-work/dist/x86_64-linux-tinfo6/Cabal-3.0.1.0/build/concordium-client/concordium-client /concordium-client
COPY --from=build /build/extra-libs/* /usr/lib/
COPY --from=build /build/scripts/start.sh /start.sh
COPY --from=build /build/*-bakers.tar.gz /
COPY --from=build /build/non-baker-accounts.json /non-baker-accounts.json

RUN chmod +x /start.sh

ENTRYPOINT ["/start.sh"]
