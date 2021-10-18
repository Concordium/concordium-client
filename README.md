# Concordium Client

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.0-4baaaa.svg)](https://github.com/Concordium/.github/blob/main/.github/CODE_OF_CONDUCT.md)

Concordium Client is a command line tool for interacting with a Concordium node.

The tool has commands to

* deploy, initialize, and interact with smart contracts,
* query data from the chain,
* query the state of the consensus protocol, and
* inspect and manage the node.

## Prerequisites

* Install the Haskell tool Stack and compiler GHC via GHCUP:
   * `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`

* Install the [protoc](https://github.com/google/proto-lens/blob/master/docs/installing-protoc.md) tool for generating protobuf files:
   * MacOS: `brew install protobuf`
   * Ubuntu 19.10: `sudo apt install protobuf-compiler`

* Install development libraries for PostgreSQL and LMDB:
   * MacOS: `brew install postgresql lmdb`
   * Ubuntu 19.10: `sudo apt install libpq-dev postgresql-common liblmdb-dev` 

* Initialize submodule dependencies after cloning (`git submodule update --init --recursive`).

## Build

``` sh
stack build
```

## Usage

Run using `stack run concordium-client -- [BACKEND] COMMAND [ARGS...]`, where

* `BACKEND` is the GRPC server on which to perform the actions/queries.
  It's specified using the flags `--grpc-ip`, `--grpc-port`, and `--grpc-target`
  (might be needed when calling through proxy like, say, on the testnet).
  Nearly all commands require a backend to be specified. The exemptions being
  commands that only alter local configuration files. 

* `COMMAND` is one of the commands listed in [Commands.md](./Commands.md).

* `ARGS` is the list of arguments provided to `COMMAND`.

## Run tests

``` sh
stack test
```

## Contributing

Pull requests are welcome. For major changes, please open an issue first to
discuss what you would like to change.

Feel free to check our [issues-page](https://github.com/Concordium/concordium-client/issues).

## License
Copyright © 2021 [Concordium](https://concordium.com/).

This project is licensed with [Apache 2.0](LICENSE).

