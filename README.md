# Concordium Client

[![Contributor Covenant](https://img.shields.io/badge/Contributor%20Covenant-2.0-4baaaa.svg)](https://github.com/Concordium/.github/blob/main/.github/CODE_OF_CONDUCT.md)

### Table of contents

- [Introduction](#introduction)
- [Prerequisites](#prerequisites)
- [Build](#build)
- [Usage](#usage)
  - [Commands](#commands)
  - [Concepts and configuration](#concepts-and-configuration)
- [Contributing](#contributing)
- [License](#license)

## Introduction

Concordium Client is a command line tool for interacting with a Concordium node.

The tool has commands to

* deploy, initialize, and interact with smart contracts,
* view and manage local configurations such as aliases for accounts, modules and contracts,
* query data from the chain,
* query the state of the consensus protocol, and
* inspect and manage the node.

## Prerequisites

* Install the Haskell tool Stack:
   * Via [GHCup](https://www.haskell.org/ghcup/):`curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh`
   * Or, by itself: `curl -sSL https://get.haskellstack.org/ | sh`

* Install Rust version 1.53+:
   * `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`

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

* `COMMAND` is a command from one of the categories described below in [Commands](#commands).

* `ARGS` is the list of arguments provided to `COMMAND`.

Whenever a command takes an optional `--block` parameter, it always defaults to
the current ["best" block](#best-block).

For detailed and up to date descriptions of the commands and arguments, please
[read our documentation](https://developer.concordium.software/), or use the
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

#### Configuration

Accounts, keys, module- and contract-names may be stored in config files on disk
to avoid having to pass it as command line options.
The config directory can be specified with the `--config PATH` option and
defaults to `$XDG_CONFIG_HOME/concordium`.
The variable `XDG_CONFIG_HOME` is defined by the
[XDG standard](https://hackage.haskell.org/package/directory-1.3.6.0/docs/System-Directory.html#v:XdgConfig)
as the location of user specific configuration.
If not set or empty, it has the following system-dependent defaults:

* Unix: `$HOME/.config`
* Windows: `%APPDATA%` (`C:\Users\<user>\AppData\Roaming`)

The expected structure inside the config directory is

```
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
