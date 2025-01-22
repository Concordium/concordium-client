# It seems that linker errors occur on alpine:3.19, which seem to be related to
# (rust) libc and musl, though it is not clear exactly why.
FROM alpine:3.19

ENV PATH="${PATH}:/root/.cargo/bin:/root/.stack/bin"

COPY . /build

RUN apk add perl g++ make protoc ncurses ncurses-dev zlib zlib-static zlib-dev git wget postgresql-dev gmp-dev gmp xz

ARG RUST_VERSION
RUN wget -qO - https://sh.rustup.rs | sh -s -- --profile minimal --default-toolchain ${RUST_VERSION} -y

ARG GHC_VERSION
RUN wget -q https://s3-eu-west-1.amazonaws.com/static-libraries.concordium.com/ghc-${GHC_VERSION}-x86_64-unknown-linux-integer-gmp.tar.xz && \
        tar -xf ghc-${GHC_VERSION}-x86_64-unknown-linux-integer-gmp.tar.xz && \
        cd ghc-${GHC_VERSION}-x86_64-unknown-linux && \
        ./configure && \
        make install && \
        cd .. && \
        rm -rf ghc-${GHC_VERSION}-x86_64-unknown-linux-integer-gmp.tar.xz ghc-${GHC_VERSION}-x86_64-unknown-linux

ARG STACK_VERSION
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
RUN ["./build-distributable-concordium-client.sh"]
