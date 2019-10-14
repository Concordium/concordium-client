FROM ubuntu as build

COPY . /build
COPY concordium.proto /build/proto/concordium.proto
COPY *-bakers.tar.gz /build/

WORKDIR /build

RUN ./scripts/build-tx-generator.sh

FROM ubuntu

COPY --from=build /build/.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/tx-generator/tx-generator /tx-generator
COPY --from=build /build/.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/simple-client/simple-client /simple-client
COPY --from=build /build/extra-libs/* /usr/lib/
COPY --from=build /build/scripts/start.sh /start.sh
COPY --from=build /build/*-bakers.tar.gz /

RUN chmod +x /start.sh

ENTRYPOINT ["/start.sh"]
