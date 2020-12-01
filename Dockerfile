FROM rust:1.45.2 AS build-libs
COPY ./deps/crypto/rust-src /build
WORKDIR /build
# Note that the shared libraries build here depend on a handful of system libraries.
# All images in this this file must satisfy these dependencies.
RUN cargo build --release && mkdir /out && cp target/release/*.so /out/

FROM fpco/stack-build:lts-16.20 AS build-bin
COPY . /build
WORKDIR /build
# Copy libs from the Rust part of 'crypto' to where the Haskell part expects to find them.
COPY --from=build-libs /out deps/crypto/rust-src/target/release
# When building the Haskell part of 'crypto' it wants to call 'cargo'
# to compile the rust libs again (see Setup.hs).
# As that has already been done there's nothing to do,
# so it only needs to have a command named 'cargo' on PATH that doesn't fail
# (if it isn't there, the build just dies without any explanation).
RUN ln -s /bin/true /usr/local/bin/cargo
# 'simple-client' expects to find the same libs as 'crypto' at ./extra-libs,
# but overriding LD_LIBRARY_PATH does the same.
RUN LD_LIBRARY_PATH=deps/crypto/rust-src/target/release \
      stack build --flag "simple-client:-middleware" && \
    cp "$(stack path --local-install-root)/bin/concordium-client" /concordium-client

FROM ubuntu:20.04
COPY --from=build-bin /concordium-client /concordium-client
COPY --from=build-libs /out/* /lib/
WORKDIR /work
ENTRYPOINT ["/concordium-client"]
