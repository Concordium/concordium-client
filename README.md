# Concordium Client

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.0-4baaaa.svg)](https://github.com/Concordium/.github/blob/main/.github/CODE_OF_CONDUCT.md)

## Table of contents

- [Concordium Client](#concordium-client)
  - [Table of contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Prerequisites](#prerequisites)
  - [Build](#build)
    - [MacOS Specific](#macos-specific)
    - [M1 MacOS Specific](#m1-macos-specific)
  - [Usage](#usage)
    - [Commands](#commands)
    - [Concepts and configuration](#concepts-and-configuration)
      - ["Best" block](#best-block)
      - [Account aliases](#account-aliases)
      - [Configuration](#configuration)
  - [Contributing](#contributing)
  - [License](#license)

## Introduction

Concordium Client is a command line tool for interacting with a Concordium node.

The tool has commands to

- deploy, initialize, and interact with smart contracts,
- view and manage local configurations such as aliases for accounts, modules and contracts,
- query data from the chain,
- query the state of the consensus protocol, and
- inspect and manage the node.

For more information, please [read our
documentation](https://developer.concordium.software/en/mainnet/net/references/concordium-client.html).

[Binary distributions are available for Linux, macOS, and
Windows](https://developer.concordium.software/en/mainnet/net/installation/downloads.html#concordium-client).

## Prerequisites

To build the tool from source, you need the following prerequisites:

- Install the Haskell tool Stack:
  - Via [GHCup](https://www.haskell.org/ghcup/):
    - Unix: `curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`
    - Windows: [Follow the GHCup install guide](https://www.haskell.org/ghcup/install/#installation).
  - Or, by itself:
    - Unix: `curl -sSL https://get.haskellstack.org/ | sh`
    - Windows: [Follow the Stack install guide](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

- Install Rust version 1.68+:
  - Unix: `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`
  - Windows: [Follow the Rust install
     guide](https://www.rust-lang.org/tools/install)
    - Use the `x86_64-pc-windows-gnu` toolchain by choosing it during
       installation or by running `rustup toolchain default
       stable-x86_64-pc-windows-gnu`.
  - *Recommended after installing: Set the default Rust version to 1.68 by running `rustup default 1.68`*.

- Install the [protoc](https://github.com/google/proto-lens/blob/master/docs/installing-protoc.md) tool for generating protobuf files:
  - MacOS: `brew install protobuf`
  - Ubuntu 19.10: `sudo apt install protobuf-compiler`
  - Windows: `stack exec -- pacman -S mingw-w64-x86_64-protobuf`

- Install development libraries for PostgreSQL:
  - MacOS: `brew install postgresql`
  - Ubuntu 19.10: `sudo apt install libpq-dev postgresql-common`
  - Windows: `stack exec -- pacman -S mingw-w64-x86_64-postgresql`

- Initialize submodule dependencies after cloning (`git submodule update --init --recursive`).

## Build

``` sh
stack build
```

### MacOS Specific

You may need to add the following entries to your `~/.stack/config.yaml` for the libraries installed via `brew`:

```yaml
extra-lib-dirs:
- /opt/homebrew/lib

extra-include-dirs:
- /opt/homebrew/include/
```

### M1 MacOS Specific

You may need to add the following entry to your `~/.stack/config.yaml` for the `libffi` include:

```yaml
extra-include-dirs:
- /Library/Developer/CommandLineTools/SDKs/MacOSX12.3.sdk/usr/include/ffi/
```

To determine the exact path to the `libffi` include directory, run the following command:

```sh
pkg-config --cflags libffi
```

## Test

``` sh
stack build --test
```

## Format

``` sh
fourmolu -i $(git ls-files '*.hs')
```

## Usage

Run using `stack run concordium-client -- [BACKEND] COMMAND [ARGS...]`, where

- `BACKEND` specifies the node to target with the commands. This is specified using the
  `--grpc-ip`, `--grpc-port`, and `--grpc-target` options (where the latter might be
  needed when calling through a proxy like, say, on the testnet). The IP-address must
  belong to a node running with the GRPC API V2 enabled and served on the specified port.

- `COMMAND` is a command from one of the categories described below in [Commands](#commands).

- `ARGS` is the list of arguments provided to `COMMAND`.

Whenever a command takes an optional `--block` parameter, it always defaults to
the current ["best" block](#best-block).

For detailed and up to date descriptions of the commands and arguments, please
[read our documentation](https://developer.concordium.software/en/mainnet/net/references/concordium-client.html), or use the
`--help` flag in the tool.

### Commands

The commands are grouped by topic.

- `transaction`
  - Commands for sending transactions and querying their status.
- `account`
  - Commands for inspecting and modifying accounts on the chain. Local
    configuration can be modified via `config account ...`.
- `module`
  - Commands for deploying and inspecting smart contract modules.
- `contract`
  - Commands for initializing, interacting with and inspecting smart contract instances.
- `config`
  - Commands for inspecting and changing local configuration. In particular
    regarding accounts and their names. Names for smart contract modules and
    contracts are handled in `module` and `contract`, respectively. For more
    information, read the section on [Configuration](#configuration) below.
- `consensus`
  - Commands for inspecting the chain health (branching and finalization),
    baker election and statistics, and reward/minting parameters.
- `block`
  - Commands for inspecting individual blocks.
- `baker`
  - Commands for managing your baker. Including the creation and deployment of
    baker credentials.
- `identity`
  - Commands for viewing information about identity providers and anonymity revokers.
- `raw`
  - Commands that correspond directly to the node's API. Most of these commands
    have non-raw alternatives which are more polished and thus should be preferred.

### Concepts and configuration

#### "Best" block

Commands that operate on a specific block default to the "best" block if the parameter is omitted.

There is a bit of a race condition in the way this best block is queried:
To get the best block, we make a call, and then we need a separate call to get the block info.
In the meantime, however, the best block could have been pruned due to finalization.

#### Account aliases

`concordium-client` can generate aliases of addresses. An alias for an address
can be generated with

```console
concordium-client account show-alias ACCOUNT --alias N
```

where `ACCOUNT` is either a name of the account or an account address, and `N`
is an integer between 0 and 16777215 (inclusive) (the integer can also be
specified in hex). For example

```console
concordium-client account show-alias 4oM1reP5hVqT8Krvb9c1bJffoWW4ChTYDZVmbJwGtfGpGcDo5v --alias 0x010203
```

There is support for sending transactions via an alias of an account. Each
transaction that creates a transaction supports an `--alias` option which
generates an alias for the sender address and uses it when sending transactions
instead of the given address. The `--alias` option has the same meaning as to
the `account show-alias` command.

#### Configuration

Accounts, keys, module- and contract-names may be stored in config files on disk
to avoid having to pass it as command line options.
The config directory can be specified with the `--config PATH` option and
defaults to `$XDG_CONFIG_HOME/concordium`.
The variable `XDG_CONFIG_HOME` is defined by the
[XDG standard](https://hackage.haskell.org/package/directory-1.3.6.0/docs/System-Directory.html#v:XdgConfig)
as the location of user specific configuration.
If not set or empty, it has the following system-dependent defaults:

- Unix: `$HOME/.config`
- Windows: `%APPDATA%` (`C:\Users\<user>\AppData\Roaming`)

The expected structure inside the config directory is

```console
<configDir>
├── accounts
│   ├── <account1>             # One folder per account.
│   │   ├── <credId1>          # And one folder per credential per account.
│   │   │   └── keypair0.json
│   │   └── encSecretKey.json
│   ├── <account2>
│   │   ├── <credId2>
│   │   │   ├── keypair0.json
│   │   │   ├── keypair1.json
│   │   │   └── keypair2.json
│   │   └── encSecretKey.json
│   └── names.map              # Mapping from account name to addresses.
└── contracts
       ├── contractNames.map   # Mapping from contract name to contract addresses.
       └── moduleNames.map     # Mapping from module names to module references.
```

There are three types for name aliases used in Concordium Client.

- Account names:
  - A mapping from *account names* to *account addresses*.
- Contract names:
  - A mapping from *contract names* to *contract addresses*.
- Module names:
  - A mapping from *module names* to *module references*.

The names may be used in place of the address/reference they are referring to.
The tool will then look up and use the address or reference with that name.
Note that the name maps are only consulted once and only if the
provided address/reference is invalid.
So the maps cannot be used to map `address->address`, `reference->reference`,
or `name->name`.

The tool will use the special account name `default` if an account is needed but
not provided.

## Contributing

To contribute create a new branch from main, make changes, and make a pull request.
A person familiar with the codebase should be asked to review the changes before
they are merged.

For major changes, please open an issue first to discuss what you would like to
change.

Feel free to check our [issues-page](https://github.com/Concordium/concordium-client/issues).

## License

This project is licensed under [Apache 2.0](LICENSE).
