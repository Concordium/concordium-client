# syntax=docker/dockerfile:experimental
FROM alpine

ENV PATH="${PATH}:/root/.cargo/bin:/root/.stack/bin"

COPY . /build

RUN apk add perl g++ make protoc ncurses ncurses-dev zlib zlib-static zlib-dev git wget postgresql-dev gmp-dev gmp

ARG RUST_VERSION=1.53
RUN wget -qO - https://sh.rustup.rs | sh -s -- --profile minimal --default-toolchain ${RUST_VERSION} -y

ARG GHC_VERSION=9.0.2
RUN wget -q https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/ghc-${GHC_VERSION}-x86_64-alpine-linux-integer-gmp.tar.xz && \
        tar -xf ghc-${GHC_VERSION}-x86_64-alpine-linux-integer-gmp.tar.xz && \
        cd ghc-${GHC_VERSION}-x86_64-alpine-linux && \
        ./configure && \
        make install && \
        cd .. && \
        rm -rf ghc-${GHC_VERSION}-x86_64-alpine-linux-integer-gmp.tar.xz ghc-${GHC_VERSION}-x86_64-alpine-linux

ARG STACK_VERSION=2.7.5
RUN wget -q https://github.com/commercialhaskell/stack/releases/download/v${STACK_VERSION}/stack-${STACK_VERSION}-linux-x86_64.tar.gz && \
        tar -xf stack-${STACK_VERSION}-linux-x86_64.tar.gz && \
        mkdir -p $HOME/.stack/bin && \
        mv stack-${STACK_VERSION}-linux-x86_64/stack $HOME/.stack/bin && \
        rm -rf stack-${STACK_VERSION}-linux-x86_64 stack-${STACK_VERSION}-linux-x86_64.tar.gz && \
        echo "system-ghc: true" > ~/.stack/config.yaml && \
        stack update

COPY scripts/distributables/linux-build-distributable-concordium-client.sh /build-distributable-concordium-client.sh

RUN chmod +x /build-distributable-concordium-client.sh
WORKDIR /
ENTRYPOINT ["./build-distributable-concordium-client.sh"]
