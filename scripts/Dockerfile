FROM ubuntu as build

COPY . /build
COPY concordium.proto /build/proto/concordium.proto

WORKDIR /build

RUN ./scripts/build-tx-generator.sh

FROM ubuntu

COPY --from=build /build/.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/build/tx-generator/tx-generator /tx-generator
COPY --from=build /build/extra-libs/* /usr/lib/
COPY --from=build /build/scripts/start.sh /start.sh

RUN chmod +x /start.sh

ENTRYPOINT ["/start.sh"]
