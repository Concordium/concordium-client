# Simple Client

Command line tool for interacting with a backend node participating in the P2P network.

The tool has commands to
* translate smart contract modules locally,
* query data from the chain,
* query state of the consensus protocol, and
* inspect and manage backend node.

#### Smart contracts

Smart contracts consist of:
* Some amount of GTUs (contained in value `amount`).
* Current state (contained in value `model`).
* Implementation of the functions `init` and `receive`.

The `init` and `receive` functions are invoked when the contract is initialized and "invoked", respectively.
Both functions may update `model` and/or perform transactions to change `amount`.

The functions of the smart contract are defined in an Rust module.
Such a module may define multiple contracts and also types and other functions used by the contracts.

Smart contracts are deployed and instantiated (with some initial state) using the special transaction types
`DeployModule` and `InitContract`.

#### Nonce

All accounts have a "nonce" counter (starting from 1) which is incremented for each successfully committed transaction sent from the account.

All transactions must include the current nonce value for the sending account to ensure that transactions
are ordered and that two otherwise identical transactions can be distinguished (i.e. it guarantees that the transactions have different hashes).

The current nonce for accont `ACCOUNT-ID` can be retrieved using the command `GetAccountInfo ACCOUNT-ID` (grep for `accountNonce`).

If a transaction with a reused nonce is submitted, the following helpful error message is returned:
```
simple-client: user error (gRPC response error: Got non-success response from FFI interface Stale)
```

The nonce must be provided if a transaction is created from a (JSON) payload file (i.e. `transaction submit` or `SendTransaction`).
For `transaction send-gtu`, the nonce is resolved from the backend by default.

#### "Best" block

Commands that operate on a specific block default to the "best" block if the parameter is omitted.

There is a bit of a race condition in the way this best block is queried:
To get the best block, we make a call, and then we need a separate call to get the block info.
In the meantime the best block could have in fact been pruned due to finalization.

## Configuration

Accounts and keys may be stored in config files on disk to avoid having to pass it as command line options.
The config directory may be set using the `--config PATH` option and defaults to `$XDG_CONFIG_HOME/concordium`.
The variable `XDG_CONFIG_HOME` is defined by the
[XDG standard](https://hackage.haskell.org/package/directory-1.3.6.0/docs/System-Directory.html#v:XdgConfig)
as the location of user specific configuration.
If not set or empty, it has the following system-dependent defaults:

* Unix: `$HOME/.config`
* Windows: `%APPDATA%` (`C:\Users\<user>\AppData\Roaming`)

The expected structure inside the config directory is

```
<config-dir>
  /concordium
    /accounts
      /names.map    # Mapping from account name to addresses (optional)
      /<address>
        /<n>.sign   # n'th sign key for account <address>
        /<n>.verify # n'th verify key for account <address>
```

The account name to address map `accounts/names.map` is a key-value file formatted as
```
<account1_name> = <account1_addr>
<account2_name> = <account2_addr>
...
```

An account's name as specified in this map may be used in place of its address in the "sender" option of transaction commands.
The tool will then look up and use the address of the account with that name.
If no account is provided, it defaults to the one with name "default".

Note that the account map is only consulted once and only if the account reference is not a valid address.
So the map cannot be used to map the address nor name of one account to another.

The key files (named by `<n>` above) must exist in pairs with numeric names and the extensions shown above.
Account names may consist of letters, numbers, and the symbols '-' and '_'.

### Initialization

The adhoc script `scripts/init-config.sh <address>` initializes this config structure with `<address>` as the default account.
After confirming the directory to be initialized (which includes nuking it),
the script expects a JSON string in the following format to be pasted into stdout.

```
{
    <n>: {
        "signKey": <sign-key>,
        "verifyKey": <verify-key>
    },
    ...
}
```

This is the same format as that expected by the `--keys` flag of the [`transaction send-gtu`](#transaction-send-gtu-flags) command.
The script is a temporary solution and will be replaced with proper commands.

*Example*

```
$ scripts/init-config.sh 4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y
Press ENTER to confirm writing config to /home/testuser/.config/concordium (or Ctrl-C to cancel):
Writing '/home/testuser/.config/concordium/accounts/names.map'.
Input keys in the same JSON format as the one expected by the --keys flag.
{
        "0": {
            "signKey": "b2217dd42238d5c16d2f3308ceefed96c1316d5f70d682877f2480fdbb15afe9",
            "verifyKey": "414518b56ae0bbef0802c39eb80b5dccbf7bd557779303a8a467ab3f7d5ac07a"
        },
        "1": {
            "signKey": "8097175288884c429428bf828462d1a29a7845b005c248bb7f2c709b6afbffb3",
            "verifyKey": "0ce48959ee09e8d5c178f718f876731b6164f39ff23865e134bb86a41b893664"
        },
        "2": {
            "signKey": "10a117dde270c9c38921602290f36f8863653db807881b9b09c4cea578a0a22a",
            "verifyKey": "3c3a341f906a9cb6a5e204d216df34d2d540001762dfe802ddef29972cfbd150"
        }
    }
^D
Writing '/home/testuser/.config/concordium/accounts/4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y/0.sign'.
Writing '/home/testuser/.config/concordium/accounts/4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y/0.verify'.
Writing '/home/testuser/.config/concordium/accounts/4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y/1.sign'.
Writing '/home/testuser/.config/concordium/accounts/4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y/1.verify'.
Writing '/home/testuser/.config/concordium/accounts/4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y/2.sign'.
Writing '/home/testuser/.config/concordium/accounts/4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y/2.verify'.
```

## Prerequisites

* Install the [protoc](https://github.com/google/proto-lens/blob/master/docs/installing-protoc.md) tool for generating protobuf files
  (`sudo apt install protobuf-compiler` on Ubuntu 19.10).

* Install development libraries for PostgreSQL and LMDB (`sudo apt install libpq-dev postgresql-common liblmdb-dev` on Ubuntu 19.10).

* Initialize submodule dependencies after cloning (`git submodule update --init --recursive`).

## Usage

Run using `stack run simple-client -- [BACKEND] COMMAND [ARGS...]`, where

* `BACKEND` is the GRPC server on which to perform the actions/queries.
  It's specified using the flags `--grpc-ip`, `--grpc-port`, and `--grpc-target`
  (might be needed when calling through proxy like, say, on the testnet).
  All commands except for `LoadModule` and `ListModules` require a backend to be specified.

* `COMMAND` is one of the commands listed below.

* `ARGS` is the list of arguments provided to `COMMAND`.

All commands except for `LoadModule` and `ListModules` require a backend to be specified.

When running a local test network (using `docker-compose`), use `--grpc-ip localhost` and find the port of a backend node using `docker port <container>`.

I.e.,

```
--grpc-ip localhost
--grpc-port "$(docker port p2p-client_baker_1 | cut -d: -f2)"
```

This is wrapped up into the script `run.sh` such that one just have to do

```
./run.sh NODE-ID COMMAND [ARGS]
```

where `NODE-ID` is the container number of the node in the docker-compose cluster (just use `1`).

## Commands

The commands supported by the tool are divided into two categories: "new" and "legacy".
As the names indicate, the "new" commands are intended to replace the "legacy" ones once they're fully implemented.

### New commands

The new commands are grouped by topic. Whenever a command takes an optional block it always defaults to the current "best" block.

#### Configuration

##### `config show`

Show all persisted configuration.

*Example*

```
$ simple-client config show
Base configuration:
- Verbose:            no
- Account config dir: /home/testuser/.config/concordium/accounts
- Account name map:
    default -> 3urFJGp9AaU62fQ3DEfCczqJwVt9V3F1gjE5PPBaYgqBD6rqPB

Account keys:
- '3urFJGp9AaU62fQ3DEfCczqJwVt9V3F1gjE5PPBaYgqBD6rqPB'
    0: sign=ae3b81da56ab6e556d64abe4beac734e8b257e8dc9d3296e3a8d274208965b28, verify=a237029bdada86f07992ccdaca6d52b3ca27ef3e27444b23270587e117872dba
    1: sign=ed6f68990129dafccd47daf903f8f59ca7d6554302f02e3a7faa9f7d06032fd8, verify=e76bd607e9b25f3a2005da4e8ecf1fb6a304efa15b7edaaa59ec02e72f5a43f4
    2: sign=329bb019222c721734f1bfc22d5d30320fe120a2dd33a202a1f1a29b76d9593f, verify=57d18474625f4e4b677ed0cda28725c6e4ce9617c63afbaac08e85c4faa41208
```

#### Transaction

Transaction sending and inspection.

##### `transaction submit PAYLOAD-FILE`

*Example*

```
$ simple-client transaction submit test/transactions/transfer.json
Transaction sent to the baker.
Query status using 'transaction status 1b5aedf75e397f2c3fed3f13f96ee12d99952928ef5a0cd18bd39841b7b34430'.
```

Low-level command to send transaction from JSON payload.

##### `transaction status TX-HASH`

Poll status on the transaction `TX-HASH`.

*Example*

```
$ simple-client transaction status 1b5aedf75e397f2c3fed3f13f96ee12d99952928ef5a0cd18bd39841b7b34430
Transaction is finalized into block 180ab359d341576e90a23c08302fb78a63e671e0adedb29c306a7c12815c37e1 with status "success" and cost 165 GTU (165 NRG).
```

##### `transaction send-gtu FLAGS...`

Send a Transfer transaction.

Required flags:

* `--amount`: Amount of GTU to send.
* `--receiver`: Address of the receiver account.
* `--expiry`: Expiry time of the transaction (Unix epoch timestamp).
* `--energy`: Amount of NRG to allocate to the transaction.

Optional flags (will attempt to resolve from persistent configuration):

* `--sender`: Name or address of the sender account.
* `--keys`: Keys of the sender account.

See the [Configuration](#configuration) section for descriptions of account name and the format of `--keys`.

After signing and sending the "transfer" transaction, the command follows the life cycle of the transaction.
Unless it gets interrupted (which doesn't prevent the transaction from progressing),
the command blocks, waiting for the transaction to get committed and ultimately finalized.

*Example*

```
$ simple-client transaction send-gtu --amount 100 --energy 10000 --expiry 9999999999 --receiver 3EmnjMy8AY5zoebNaA3HuVx1UShdW8vh9n1YjJztmSc2jN4K3V
Account reference not provided; using "default".
Sending 100 GTU from '4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y' (default) to '3EmnjMy8AY5zoebNaA3HuVx1UShdW8vh9n1YjJztmSc2jN4K3V'.
Allowing up to 10000 NRG to be spent as transaction fee.
Confirm [yN]: y
Transaction sent to the baker. Waiting for transaction to be committed and finalized.
You may skip this by interrupting this command (using Ctrl-C) - the transaction will still get procesed and may be queried using 'transaction status eb7eda074456ab868eb544ac8ffaf3ec84825b76c111d8fa5c67de30d8c3695f'.
[15:50:26] Waiting for the transaction to be committed.....
Transaction is committed into block 7b33b3593e3100b38e6f70cd2a8a942116c99c000ea6011ff2af4707dec2a011 with status "success" and cost 165 GTU (165 NRG).
[15:50:30] Waiting for the transaction to be finalized......
[15:50:45] Transfer completed.
```

#### Account

Account inspection.

##### `account show ADDRESS [--block BLOCK-HASH]`

Show details of a specific account as of a specific block.

*Example*

```
$ simple-client account show 2zgcMk7heVZKaaBfPtxVqmvE3GnrrP7N2nFGHoiC6X9nZT9TaG --block 7b33b3593e3100b38e6f70cd2a8a942116c99c000ea6011ff2af4707dec2a011
Address:    2zgcMk7heVZKaaBfPtxVqmvE3GnrrP7N2nFGHoiC6X9nZT9TaG
Amount:     1000000263826 GTU
Nonce:      1
Delegation: 3

Credentials:
* 88d28b6922c4e63fab6b34f0a4ae42d33817102f96d5da20fab7d0a449b92399aef395a87dc587aa799cd202fcb75c90:
  - Expiration: Thu, 18 Mar 2021 08:40:53 UTC
  - Revealed attributes: none
```

##### `account list [--block BLOCK-HASH]`

Show list of all account addresses as of a specific block.

*Example*

```
$ simple-client account list --block 7b33b3593e3100b38e6f70cd2a8a942116c99c000ea6011ff2af4707dec2a011
2zgcMk7heVZKaaBfPtxVqmvE3GnrrP7N2nFGHoiC6X9nZT9TaG
33MT5V7LAzbjfRS537nqq9AqHb4ALymQWwYhriE3kcYr8qsDoS
3EmnjMy8AY5zoebNaA3HuVx1UShdW8vh9n1YjJztmSc2jN4K3V
3GqF3FQPxyxUChnBcwsC5jmY4kqWyi6R8dNXHk88GYqCPKaLGP
3HiDcMNQvWgzeC7NvvNLsajumazeBhStvaj2vdS9aTuKRyTP3a
3QHhwuM28e9w8azXqoChqPy56vgJfCEuQHRjpzoXv44kALXfiC
3ZFGxLtnUUSJGW2WqjMh1DDjxyq5rnytCwkSqxFTpsWSFdQnNn
3eAdHh7atmvo7wcQqgp7Ft1TAU9AhGjTxyjxuTYJqpVc8Uq1SV
3iXopWKixXBegpj6acM6vcj2SJeG6Tbo4eDdSahX62Rz58Td5q
3rqX3VeBrshxHnhj5Vt1CMT9J55ybCGd6avSea5oNFLPB65pC2
4A7L8pPDinfQnUkunEzAYcAYBH5MAFthrbA3NFUq2Ge5fbsnPd
4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y
4SS2jUt2Nj7vJuWu3TBCYp1etr47qJPtm3dVyJDHGz751fRpph
4TJTbQEAfKfqva9CspS4XLCDYaHamvk8nqHXtQqmbdf2dygwwj
4dxjX98YTMvhZtpVKPorbVPao6GtyJFotRwEwpTHKhPa3fARFn
```

#### Consensus

Inspection of chain health and election parameters.

##### `consensus status`

List various parameters related to the current state of the consensus protocol.

*Example*

```
$ simple-client consensus status
Best block:                  7f9641fd4dfc1ffca2ef187fdddff375bb975764d66d68744574b893b61a8338
Genesis block:               1c647ab5e7ff63b28926f5eed88a9d49b130942a54d791abfa79b4cc0c98acd0
Last finalized block:        183e50fb2700716bd6f194f62fbd4b142a657b4bbd6d83bb64093463960ba4f3
Best block height:           154
Last finalized block height: 153
Blocks received count:       128
Block last received time:    Wed, 18 Mar 2020 14:57:45 UTC
Block receive latency:          60 ms (EMA),    39 ms (EMSD)
Block receive period:         7812 ms (EMA),  9086 ms (EMSD)
Blocks verified count:       171
Block last arrived time:     Wed, 18 Mar 2020 14:57:45 UTC
Block arrive latency:           61 ms (EMA),    60 ms (EMSD)
Block arrive period:          5029 ms (EMA),  6388 ms (EMSD)
Transactions per block:          0 ms (EMA),     3 ms (EMSD)
Finalization count:          51
Last finalized time:         Wed, 18 Mar 2020 14:57:50 UTC
Finalization period:         17434 ms (EMA), 11541 ms (EMSD)
```

##### `consensus show-parameters [--block BLOCK-HASH] [--include-bakers]`

Show election parameters as of a specific block, optionally including the "lottery power" of individual bakers.

*Example*

```
$ simple-client consensus show-parameters --include-bakers
Election nonce:      758d81a016e747f186a1fd3706f267c07d8e900d0126c08adfd89e34a77eb92c
Election difficulty: 0.2
Bakers:
                             Account                       Lottery power
        ----------------------------------------------------------------
     0: 4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y    0.1998
     1: 3EmnjMy8AY5zoebNaA3HuVx1UShdW8vh9n1YjJztmSc2jN4K3V    0.1998
     2: 33MT5V7LAzbjfRS537nqq9AqHb4ALymQWwYhriE3kcYr8qsDoS    0.1998
     3: 2zgcMk7heVZKaaBfPtxVqmvE3GnrrP7N2nFGHoiC6X9nZT9TaG    0.1997
     4: 3rqX3VeBrshxHnhj5Vt1CMT9J55ybCGd6avSea5oNFLPB65pC2    0.2009
```

#### Block

Block inspection.

##### `block show [BLOCK-HASH]`

Show stats for a block.

*Example*

```
$ simple-client block show 7f9641fd4dfc1ffca2ef187fdddff375bb975764d66d68744574b893b61a8338
Hash:                       7f9641fd4dfc1ffca2ef187fdddff375bb975764d66d68744574b893b61a8338
Parent block:               183e50fb2700716bd6f194f62fbd4b142a657b4bbd6d83bb64093463960ba4f3
Last finalized block:       a4e69297aa654e716cb9f52170aa7639d1cf03c83c87e26e4f8109da11761cbb
Finalized:                  yes
Receive time:               Wed, 18 Mar 2020 14:57:45 UTC
Arrive time:                Wed, 18 Mar 2020 14:57:45 UTC
Slot:                       63921465
Slot time:                  Wed, 18 Mar 2020 14:57:45 UTC
Transaction count:          0
Transaction energy cost:    0 NRG
Transactions size:          0
Transaction execution cost: 0 GTU
Total amount:               15006392146500 GTU
Total encrypted amount:     0 GTU
Central bank amount:        368 GTU
Minted amount per slot:     100 GTU
```

### Legacy Commands

All the commands supported by the client are documented below.
See [this wiki page](https://gitlab.com/Concordium/notes-wiki/wikis/Consensus-queries#state-queries)
for a reference on the output values.

In the examples, the dummy binary `simple-client` represents whatever method is used to invoke the client.

The transaction payload listed using `cat` are all shown to have nonce 1.
They need to be replaced with the next nonce of the sending account as described in the [nonce](#nonce) section.

#### `LoadModule FILE`

Parses the acorn source file `FILE` and replaces module imports by their IDs.
Unless they're builtins, these modules have to be already loaded.
A binary representation of the resulting AST is then stored into a "database" (contained in a binary file `.cache`) on the local client.

There is currently no command for getting back the "translated" module from the local client.

The command doesn't use the backend, so any `--grpc-*` flags are ignored.

*Example*

```
$ simple-client LoadModule ../prototype/scheduler/test/contracts/SimpleCounter.acorn
Module processed.
The following modules are currently in the local database and can be deployed.

Module SimpleCounter with reference 0433d69fd2974e90a5e9eb81d214f10623e4771bd2767c7fee7f373385d24086
```

#### `ListModules`

Lists all modules which have been loaded on the local client.

The command doesn't use the backend, so any `--grpc-*` flags are ignored.

*Example*

```
$ simple-client ListModules
The following modules are in the local database.

Module SimpleCounter with reference 0433d69fd2974e90a5e9eb81d214f10623e4771bd2767c7fee7f373385d24086
```

#### `SendTransaction FILE [--hook]`

Send transaction defined in the JSON file `FILE` on net `NET-ID` (after signing it).
If the `--hook` flag is provided, a hook is automatically installed.
If not, it may be installed using the command `HookTransaction` though it will only work
if it's called before the transaction is processed.

Supported transaction types (see the `simple-client/test/transactions/` directory for examples):

* `DeployModule` (example below)
* `InitContract` (example under `GetInstances`)
* `Update` (example under `GetInstanceInfo`)
* `Transfer`
* `RemoveBaker`
* `UpdateBakerAccount`
* `UpdateBakerSignKey`
* `DelegateStake`

*Example: Deploy module "SimpleCounter"*

```
$ cat test/transactions/deploy.json 
{
    "sender": "3M3cAqFN3MzeaE88pxDys7ia6SFZhT7jtC4pHd9AwoTBYSVqdM",
    "keys": {
        "0": {
            "signKey": "e83998691848313ec357a7c9f11f4ff9153dfc001e4592e37cd5fcdd1a0a9728",
            "verifyKey": "19bf1669f814e52ca1dc1cca4fe066a6019030746a2204bb03f48cc1ea417adf"
        },
        <...>
    },
    "nonce": 1,
    "energyAmount": 1000000,
    "payload": {
        "transactionType": "DeployModule",
        "moduleName" : "SimpleCounter"
    }
}
$ simple-client SendTransaction test/transactions/deploy.json --hook
Installing hook for transaction 88b123dbf59969bf4e5f05543217385f154b5029f6ebb8b4af9fe3b682e88136
{
    "status": "absent",
    "results": [],
    "expires": "2020-01-23T13:19:08.7107675Z",
    "transactionHash": "88b123dbf59969bf4e5f05543217385f154b5029f6ebb8b4af9fe3b682e88136"
}
Transaction sent to the baker. Its hash is 88b123dbf59969bf4e5f05543217385f154b5029f6ebb8b4af9fe3b682e88136
```

#### `HookTransaction TX-HASH`

Installs a "hook" on a previously submitted transaction and inspects its progress.
If the hook is already installed (i.e. if `--hook` was passed to `SendTransaction` or
`HookTransaction` has already been called), the command just outputs the transaction state.

Output format: JSON (object).

*Example*

Right after sending the transaction in the example above:

```
$ simple-client HookTransaction 88b123dbf59969bf4e5f05543217385f154b5029f6ebb8b4af9fe3b682e88136
{
    "status": "pending",
    "results": [],
    "expires": "2020-01-23T13:20:28.32144163Z",
    "transactionHash": "88b123dbf59969bf4e5f05543217385f154b5029f6ebb8b4af9fe3b682e88136"
}
```
A few minutes later:

```
$ simple-client HookTransaction 88b123dbf59969bf4e5f05543217385f154b5029f6ebb8b4af9fe3b682e88136
{
    "status": "committed",
    "results": [
        {
            "blockHash": "e652f34f2444b747d16c7d20f158a68e31afcddab3aedc78989ecb8d0870daa4",
            "result": "success",
            "events": [
                "ModuleDeployed 0433d69fd2974e90a5e9eb81d214f10623e4771bd2767c7fee7f373385d24086"
            ],
            "executionEnergyCost": 780,
            "executionCost": 780
        }
    ],
    "expires": "2020-01-23T13:22:09.169639748Z",
    "transactionHash": "88b123dbf59969bf4e5f05543217385f154b5029f6ebb8b4af9fe3b682e88136"
}
```

Eventually, the status should (hopefully) change to `"finalized"`.

#### `GetConsensusInfo`

Retrieves the current state of the consensus layer. The fields are documented in [`ConsensusStatus`](https://gitlab.com/Concordium/notes-wiki/-/wikis/Consensus-queries#getconsensusstatus-consensusstatus).

Output format: JSON (object).

*Example*

```
$ simple-client GetConsensusInfo
{
    "lastFinalizedBlockHeight": 76,
    "blockArriveLatencyEMSD": 3.258284834322888e-2,
    "blockReceiveLatencyEMSD": 3.0965830000556763e-2,
    "lastFinalizedBlock": "0ddfcfbc08df93515ec02d7493576cb0985faffd23e5a1bbd61d105413a866e1",
    "blockReceivePeriodEMSD": 40.12266599756365,
    "blockArrivePeriodEMSD": 39.55843910022231,
    "blocksReceivedCount": 70,
    "transactionsPerBlockEMSD": 2.2696076902036087e-2,
    "finalizationPeriodEMA": 205.66920982927851,
    "bestBlockHeight": 79,
    "lastFinalizedTime": "2020-01-23T13:47:15.297486301Z",
    "finalizationCount": 19,
    "blocksVerifiedCount": 87,
    "finalizationPeriodEMSD": 82.12333829540961,
    "transactionsPerBlockEMA": 5.153775207320114e-4,
    "blockArriveLatencyEMA": 6.420760645925422e-2,
    "blockReceiveLatencyEMA": 6.123876636028691e-2,
    "blockArrivePeriodEMA": 47.95363481191248,
    "blockReceivePeriodEMA": 50.44551949862284,
    "blockLastArrivedTime": "2020-01-23T13:48:50.069962947Z",
    "bestBlock": "20c93296260f08f60486c191cd718013cf6bd6e6a5edf207af964a43e56b8d3d",
    "genesisBlock": "32b65722dfd4df1b4a8f693128f135ad92a3d8fdfab1d7d6b6e78f18ccdc9e94",
    "blockLastReceivedTime": "2020-01-23T13:48:50.063678988Z"
}
```
    
#### `GetBlockInfo [BLOCK-HASH]`

Retrieves information on block `BLOCK-HASH` or the current best block if none was provided.

Output format: JSON (object). Outputs `null` if the provided hash is invalid.

*Example*

```
$ /simple-client GetBlockInfo
{
    "transactionsSize": 0,
    "blockParent": "d2f566d3acc47d824e0c902833536c33f2af07855002d1060d883f0b37e4f3cf",
    "mintedAmountPerSlot": 100,
    "totalEncryptedAmount": 0,
    "blockHash": "32244ea5eaeb13ec4a59aad95e92a9f46eb0177b49c2011cbbc4f46ebd1d698d",
    "finalized": false,
    "totalAmount": 15000591656600,
    "blockArriveTime": "2020-01-23T13:54:20.043906231Z",
    "blockReceiveTime": "2020-01-23T13:54:20.040554227Z",
    "transactionCount": 0,
    "transactionEnergyCost": 0,
    "blockSlot": 5916566,
    "blockLastFinalized": "dc534d3de795ebf9856d00dbc927c39a5b08eaf5a447d192cf8abec122995bf9",
    "blockSlotTime": "2020-01-23T13:54:20Z",
    "blockHeight": 83,
    "blockBaker": 3,
    "executionCost": 0,
    "centralBankAmount": 618
}
$ simple-client GetBlockInfo dc534d3de795ebf9856d00dbc927c39a5b08eaf5a447d192cf8abec122995bf9
{
    "transactionsSize": 0,
    "blockParent": "20c93296260f08f60486c191cd718013cf6bd6e6a5edf207af964a43e56b8d3d",
    "mintedAmountPerSlot": 100,
    "totalEncryptedAmount": 0,
    "blockHash": "dc534d3de795ebf9856d00dbc927c39a5b08eaf5a447d192cf8abec122995bf9",
    "finalized": true,
    "totalAmount": 15000591655500,
    "blockArriveTime": "2020-01-23T13:52:30.007253268Z",
    "blockReceiveTime": "2020-01-23T13:52:30.007205245Z",
    "transactionCount": 0,
    "transactionEnergyCost": 0,
    "blockSlot": 5916555,
    "blockLastFinalized": "0ddfcfbc08df93515ec02d7493576cb0985faffd23e5a1bbd61d105413a866e1",
    "blockSlotTime": "2020-01-23T13:52:30Z",
    "blockHeight": 80,
    "blockBaker": 4,
    "executionCost": 0,
    "centralBankAmount": 1344
}
$ simple-client GetBlockInfo something-invalid
null
```

#### `GetAccountList [BLOCK-HASH]`

Retrieves the IDs of all accounts on a specific (or the current best) block.

Output format: JSON (list).

*Example*

```
$ simple-client GetAccountList
[
    "356XG1CpfGhnhCbVYynFoicssQmXFmV11gBTasKT7nErapsCK2",
    "3M3cAqFN3MzeaE88pxDys7ia6SFZhT7jtC4pHd9AwoTBYSVqdM",
    "3M7o7ssGYMCjCnwjpykoXAHjNghGZf5rbUtXp1bwFQzmvwrfHp",
    "3XwJuZ7bEMdTdFKvM5A5LaMGYuiem1HyvE2tJhSUr1sw44HNke",
    "3Zz826dPyUc3ieyp111HpQMxD9ZLZQzMZJT5GcbDFjsAyYgX9a",
    "3jW1WGJr3nTRYFBkrYt4sGRY1bBGdcFkN8hngZhpy3hW9n9sxj",
    "3sYHD5YAQu3f7bKF4NCgm4R4L7FzxBNU5GLmHvhpoz19Jsdyx1",
    "3u6Vadj5vYeJQPgmszqxHccoDo1i5AbMxZpvgbz9YgQzfKWEcq",
    "3yLVYWVp15CrCLeEfwwsR2t8sNyoNAYx3gGhw5F8yEqpT3ua1u",
    "427hfRGrR45RoHTNRW7FVL2H5GTD7aowknzu4hvo1gkXgqdvKU",
    "43TBz1y9uoKA6zzJUK7LnfDkFfJxPzyzQwArxrGg346PQx56fK",
    "4LQXhLTy6nLdhmYiyH5MfT6CiTRUjpMAhxJwaRS4CdXH9YNCm5",
    "4cQ9w4oE3CxsCEc4YBiPN6jnR4EbTDVcqGM9bwuSPLk6mX6D3p",
    "4ekKVNZUmjdoe3rZBvPcM3XvbAymCVA9G5NHpcsrWpGeKZ6waC",
    "4nkQ8fdzjDnhmzp1YvkRykyXKFv2xm6mDVU3PKqhD2qA14hgCT"
]
```

#### `GetAccountInfo ACCOUNT-ID [BLOCK-HASH]`

Retrieves information of a specific account on a specific (or the current best) block.

Output format: JSON (object).

*Example*

```
$ simple-client GetAccountInfo 356XG1CpfGhnhCbVYynFoicssQmXFmV11gBTasKT7nErapsCK2
{
    "accountAmount": 1000000000000,
    "accountNonce": 1,
    "accountDelegation": null,
    "accountCredentials": [
        [
            1611220131,
            {
                "ipIdentity": 0,
                "regId": "b7d9cce70d7f117701f14e867b7c95e802fa9063a41adb132f1dbd3c39f2be29658e1351a5132577bf1d81fec31a7ad8",
                "arData": [
                    {
                        "idCredPubShareNumber": 1,
                        "encIdCredPubShare": "99377312d47d1041acbccdea15b6cc38c821538ed8f582f328782616b47816057c4bfec656b80e9282d1d0401e016e8187301197952aea87f74301fde9fe71a31ca95cdb103f5869deedb2b5af08780162cfed9fa3ffd27460979f3a85af300e",
                        "arIdentity": 0
                    },
                    {
                        "idCredPubShareNumber": 2,
                        "encIdCredPubShare": "830b7c30b6d494ebbf1f109d30d3bd47ee3f0fb79041bc05dd86e18cb55b596c25f29610c20fb69e78a85bb831bb1959af0cf975b32edab12df01c70ef4571b8ff946d7b7a2cb6337dca480359f7bf2889e6655e0b2cfc9264db272596d380e8",
                        "arIdentity": 1
                    },
                    {
                        "idCredPubShareNumber": 3,
                        "encIdCredPubShare": "92fff442f4f5f720dfdff8f1d3820baecdc4e95d7cf13ec2077656eeee1ed0cca072ee7d62611df7cf172055c28f8df98065433f5a147667a740c93446de91c91ab804bfca20221d62f5b5e599364f70bdd15c4123aba5a6b01e32a16413b2d1",
                        "arIdentity": 2
                    }
                ],
                "account": {
                    "keys": [
                        {
                            "verifyKey": "2255892d325969c9087d901ec22cf054ccbedf5ee0b388968761de09f63696da",
                            "schemeId": "Ed25519"
                        },
                        {
                            "verifyKey": "ca4c22b811abb21dbfc723a0ad4a186e06da34b2f05bbf1171530e678cf3a162",
                            "schemeId": "Ed25519"
                        },
                        {
                            "verifyKey": "c51bb8c8591fc676c25cbc96e0f81aea70fd2a3c5a8327027b167828bd2cdf2d",
                            "schemeId": "Ed25519"
                        }
                    ],
                    "threshold": 2
                },
                "revocationThreshold": 2,
                "policy": {
                    "variant": 65535,
                    "expiry": 1611220131,
                    "revealedItems": []
                }
            }
        ]
    ]
}
$ simple-client GetAccountInfo invalid-account
null
```

The number `1611220131` in the result above is the expiration date of the particular credential.

#### `GetInstances [BLOCK-HASH]`

Retrieves a list of smart contract instances on the blockchain as of a specific (or the current best) block.

Output format: JSON (list).

*Example: Initialize smart contract "Counter" (from module "SimpleCounter")*

The local test network starts without any smart contract instances:

```
$ simple-client GetInstances
[]
```

Initialize contract (assuming the `SimpleCounter` module has already been deployed). Note that the nonce may need to be updated:

```
$ cat test/transactions/init.json 
{
    "sender": "3M3cAqFN3MzeaE88pxDys7ia6SFZhT7jtC4pHd9AwoTBYSVqdM",
    "keys": {
        "0": {
            "signKey": "e83998691848313ec357a7c9f11f4ff9153dfc001e4592e37cd5fcdd1a0a9728",
            "verifyKey": "19bf1669f814e52ca1dc1cca4fe066a6019030746a2204bb03f48cc1ea417adf"
        },
        <...>
    },
    "nonce": 1,
    "energyAmount": 1000000,
    "payload": {
        "transactionType": "InitContract",
        "amount": 100,
        "moduleName" : "SimpleCounter",
        "contractName": "Counter",
        "parameter": "0"
    }
}
$ simple-client SendTransaction test/transactions/init.json --hook
Installing hook for transaction 84a3636a059bbbfe5927dd421ce0b39ec568025bbbd895381c02d1b9ef9fbfa0
{
    "status": "absent",
    "results": [],
    "expires": "2020-01-24T10:37:07.594154573Z",
    "transactionHash": "84a3636a059bbbfe5927dd421ce0b39ec568025bbbd895381c02d1b9ef9fbfa0"
}
Transaction sent to the baker. Its hash is 84a3636a059bbbfe5927dd421ce0b39ec568025bbbd895381c02d1b9ef9fbfa0
```

A minute later:

```
$ simple-client HookTransaction 84a3636a059bbbfe5927dd421ce0b39ec568025bbbd895381c02d1b9ef9fbfa0
{
    "status": "committed",
    "results": [
        {
            "blockHash": "ce5f19e122e9a47fb66f7b4c14582a0f774c4c2ffc1a50867b008fd6daee13b6",
            "result": "success",
            "events": [
                "ContractInitialized 0433d69fd2974e90a5e9eb81d214f10623e4771bd2767c7fee7f373385d24086 (TyName 3) <0, 0>"
            ],
            "executionEnergyCost": 10376,
            "executionCost": 10376
        }
    ],
    "expires": "2020-01-24T10:37:19.054684092Z",
    "transactionHash": "84a3636a059bbbfe5927dd421ce0b39ec568025bbbd895381c02d1b9ef9fbfa0"
}
$ simple-client GetInstances
[
    {
        "subindex": 0,
        "index": 0
    }
]
```

#### `GetInstanceInfo INSTANCE-ID [BLOCK-HASH]`

Retrieves state of a smart contract instance (as of a specific or the current best block) or `null` if a non-existent/invalid one is provided.
The instance ID is given as a JSON document in the format produced by `GetInstances`.

Output format: JSON (object).

*Example*

```
$ simple-client GetInstanceInfo '{ "subindex": 0, "index": 0 }'
{
    "amount": 100,
    "owner": "3M3cAqFN3MzeaE88pxDys7ia6SFZhT7jtC4pHd9AwoTBYSVqdM",
    "model": 0
}
```

The contract was initialized with 100 GTUs and a `model` ("counter value") of 0.
The counter can be incremented using the "update" transaction:

```
$ cat test/transactions/update.json 
{
    "sender": "3M3cAqFN3MzeaE88pxDys7ia6SFZhT7jtC4pHd9AwoTBYSVqdM",
    "keys": {
        "0": {
            "signKey": "e83998691848313ec357a7c9f11f4ff9153dfc001e4592e37cd5fcdd1a0a9728",
            "verifyKey": "19bf1669f814e52ca1dc1cca4fe066a6019030746a2204bb03f48cc1ea417adf"
        },
        <...>
    },
    "nonce": 1,
    "energyAmount": 1000000,
    "payload": {
        "moduleName": "SimpleCounter",
        "message": "Inc 20",
        "transactionType": "Update",
        "amount": 0,
        "address": { "index" : 0, "subindex" : 0 }
    }
}
$ simple-client SendTransaction test/transactions/update.json --hook
...
Transaction sent to the baker. Its hash is 5fe96b57dff8113298b10c52e4a30f51339db2acc48b2341f4d9193720186a5b
$ simple-client HookTransaction 5fe96b57dff8113298b10c52e4a30f51339db2acc48b2341f4d9193720186a5b
{
    "status": "committed",
    "results": [
        {
            "blockHash": "2aea7ddbe12e40263d2af9bdd0bb6385de2704c4dce18dff677e622a083570f6",
            "result": "success",
            "events": [
                "Updated (AddressAccount 3M3cAqFN3MzeaE88pxDys7ia6SFZhT7jtC4pHd9AwoTBYSVqdM) <0, 0> 0 (ExprMessage (Let (LetForeign (Imported 0433d69fd2974e90a5e9eb81d214f10623e4771bd2767c7fee7f373385d24086 (Name 0)) (Linked (Constructor (Name 0))) (App (Reference (-1)) [Literal (Int64 20)])) (Let (Constructor (Name 1)) (App (Reference 0) [BoundVar (Reference 1)]))))"
            ],
            "executionEnergyCost": 1208,
            "executionCost": 1208
        }
    ],
    "expires": "2020-01-24T10:43:25.578864647Z",
    "transactionHash": "5fe96b57dff8113298b10c52e4a30f51339db2acc48b2341f4d9193720186a5b"
}
$ simple-client GetInstanceInfo '{ "subindex": 0, "index": 0 }'
{
    "amount": 100,
    "owner": "3M3cAqFN3MzeaE88pxDys7ia6SFZhT7jtC4pHd9AwoTBYSVqdM",
    "model": 210
}
```

Calling the contract with payload `(Inc 20)` increments `model` by `1+2+...+20 = 210`.
The invocation didn't include any GTUs, so `amount` is unchanged.

#### `GetRewardStatus [BLOCK-HASH]`

Retrieves information on how the amount of GTU in circulation and the rate at which new ones are being minted (as of a specifiec or the current best block).

The "central bank amount" increases steadily by "minted amount per slot" and determines block rewards.

The behavior is to be changed based on workshop discussions.

Output format: JSON (object).

*Example*

```
$ simple-client GetRewardStatus
{
    "mintedAmountPerSlot": 100,
    "totalEncryptedAmount": 0,
    "totalAmount": 15000592436000,
    "centralBankAmount": 471
}
```

#### `GetBirkParameters [BLOCK-HASH]`

Retrieves Birk parameters for the network as of a specific or the current best block.

The Birk parameters of a block consist of:
* `bakers`: The current bakers (i.e. the entities who are allowed to make new blocks).
* `bakerLotteryPower`: The given baker's relative likelihood of winning new blocks
*  Global parameters which influence who has the right to make a block, and when:
  * `electionNonce`: Input to the VRF function (together with baker's private election key and some other things).
  * `electionDifficulty`: The probability that a block is produced within a time slot.
    With difficulty 0.2, new blocks are expected to be baked every 5 slots on average (if all bakers are online).
    As higher value means that blocks are produced more often, it's actually the opposite of "difficulty".
    A low difficulty value should result in less branching than a high one.
   
Output format: JSON (object).

*Example*

```
$ simple-client GetBirkParameters
{
    "electionNonce": "4d1118c8c191ea21e7f6077ec068930212eabeab35b3a07927157586b175368a",
    "bakers": [
        {
            "bakerId": 0,
            "bakerLotteryPower": 0.19997686150790925,
            "bakerAccount": "3jW1WGJr3nTRYFBkrYt4sGRY1bBGdcFkN8hngZhpy3hW9n9sxj"
        },
        {
            "bakerId": 1,
            "bakerLotteryPower": 0.20000592344597554,
            "bakerAccount": "427hfRGrR45RoHTNRW7FVL2H5GTD7aowknzu4hvo1gkXgqdvKU"
        },
        {
            "bakerId": 2,
            "bakerLotteryPower": 0.20003578368326275,
            "bakerAccount": "3u6Vadj5vYeJQPgmszqxHccoDo1i5AbMxZpvgbz9YgQzfKWEcq"
        },
        {
            "bakerId": 3,
            "bakerLotteryPower": 0.20000037002572094,
            "bakerAccount": "3M7o7ssGYMCjCnwjpykoXAHjNghGZf5rbUtXp1bwFQzmvwrfHp"
        },
        {
            "bakerId": 4,
            "bakerLotteryPower": 0.1999810613371315,
            "bakerAccount": "3yLVYWVp15CrCLeEfwwsR2t8sNyoNAYx3gGhw5F8yEqpT3ua1u"
        }
    ],
    "electionDifficulty": 0.2
}
```

#### `GetModuleList [BLOCK-HASH]`

Retrieves the references of all modules loaded on the chain as of a specific or the current best block.

Output format: JSON (list).

*Example*

```
[
    "2790dc37cf1412f41ecb51be92723865520baa8e25f975ea24f0762ceca1c4a9",
    "37fd4812beb075436ad004b3ec5ab05a9823bfabdf14e9f47124c64027b89113",
    "40c62b528cd8921311ca78facdd07bbf3105e01b8f231a7ceabd00374c22700d",
    "413880f2c8f2c4ece93c806b0c541aabdcad8ce00113ef06ba191582ce36f053",
    "4408bc885226995c6ee0da4a54e30f446a5e484b666c7bb541f7336f08df90a8",
    "5f628b1cbd453ef3f600438c4d3603868d0574143c9f1aa0a96cf5f6096f1874",
    "71bf48abc6517d8f6007cd1fc12e46df82a53bc89cb54b6c22729f8606693c19",
    "801245b7db3d402bc74fd346e6d65fc940f1eba0b4856f17d8a366878f177ed4",
    "8571808367666e583b46490e19005a448ebae8dac2a691de19dde220a74548cd",
    "8eb01033ce688153a6e51970aba1ffbc3bcea26332467d9ed6201fa5b275dfe8",
    "ca4355e542005a3503a8aaaa2f36dc869d144e17af72f2faf3da3fe4107b6c86",
    "f7c12af3331b5db6d49a6253a534ea4b5f3fdbc63adbf575e1d07d836b083ab8"
]
```

#### `GetModuleSource MODULE-REF [BLOCK-HASH]`

Retrieves the "source code" of a module by reference.

Output format: A header line followed by a pretty-printed variant of the Acorn AST of the module (See `LoadModule` above).

*Example*

```
$ simple-client GetModuleSource 2790dc37cf1412f41ecb51be92723865520baa8e25f975ea24f0762ceca1c4a9
Retrieved module "2790dc37cf1412f41ecb51be92723865520baa8e25f975ea24f0762ceca1c4a9"
module where


import 71bf48abc6517d8f6007cd1fc12e46df82a53bc89cb54b6c22729f8606693c19 as $1


data DataTy($0) α =
    $0 
  | $1 α





public $2 =
  Λα.λ(x : DataTy(0) α).
       case x of
           $0  -> $1.$1
           $1 (y :: α) -> $1.$0





version 1
```

#### `GetNodeInfo`

Retrieves information on the activity of the consensus protocol and the roles that the backend node is currently having in it.

Output format: Text.

*Example*

```
$ simple-client GetNodeInfo
Node id: "0000000000000001"
Current local time: 1579859118
Peer type: "Node"
Baker running: True
Consensus running: True
Consensus type: "Active"
Baker committee member: True
Finalization committee member: True
```

#### `GetBakerPrivateData`

Retrives the baker's keys. Is currently not access controlled.

Output format: JSON (object).

*Example*

```
$ simple-client GetBakerPrivateData
{
    "signatureVerifyKey": "5c18a8ed9a86d6471c7a2915f564047cceae1b97d6344762ed83af5e7541172e",
    "aggregationSignKey": "59ee62aa69b1189ff22617a7168d2a25afe2f15fee067789b7be27dfa0b84bd7",
    "electionPrivateKey": "f33ae6e656dcf147b3f206d40e643d78deacce64686cdbcc6f51c5e3ce69e55c",
    "aggregationVerifyKey": "b668ea5af5b7ca6a79d42013c84e5611f5b0cac565f10d53c77b465656017762c725b41375efaf87343f0cc2437d464f0ab52b252d9f60909f2107850ded4f7f86becadfab0871fda9724c1ee1c9ad14cf27f1cfee3111630aff4b693a2609bb",
    "signatureSignKey": "0fbc991f8b293874d0f07332bbe532e334638c6a7b484c394da77cec11b4754a",
    "electionVerifyKey": "a9f6da4fd399a6d3964d02f2bf583fc04b82f0c3e35aa4e0d691cf057ecb42c0"
}
```

#### `GetPeerData`

Retrieves statistics on the peers known by the backend node.
The node itself is not included in this list.

Output format: Text.

*Example*

```
$ simple-client GetPeerData
Total packets sent: 416
Total packets received: 431
Peer version: 0.2.1
Peer stats:
  Peer: 0000000000000000
    Packets sent: 102
    Packets received: 106
    Measured latency: 109

  Peer: 0000000000000004
    Packets sent: 99
    Packets received: 105
    Measured latency: 104

  Peer: 0000000000000002
    Packets sent: 105
    Packets received: 107
    Measured latency: 104

  Peer: 0000000000000003
    Packets sent: 104
    Packets received: 106
    Measured latency: 104

Peer type: Node
Peers:
  Node id: 0000000000000000
    Port: 8888
    IP: 172.18.0.5

  Node id: 0000000000000004
    Port: 42524
    IP: 172.18.0.7

  Node id: 0000000000000002
    Port: 52598
    IP: 172.18.0.9

  Node id: 0000000000000003
    Port: 39636
    IP: 172.18.0.6

```

#### `StartBaker`

Output: `FAIL`.

#### `StopBaker`

Output: `OK`.

#### `PeerConnect NODE-IP NODE-PORT`

Output: `OK`.

#### `GetPeerUptime`

Retrieves the uptime of the backend node.

Output format: `Either` value marshalled from Haskell containing the uptime in ms.

*Example*

```
$ simple-client GetPeerUptime
Right 13504701
```

#### `BanNode NODE-ID NODE-PORT NODE-IP`

Doesn't seem to do anything.

Output: `"cannot parse value <ip>"`, where `<ip>` is the provided `NODE-IP`.

#### `UnbanNode NODE-ID NODE-PORT NODE-IP`

Doesn't seem to do anything.

Output: `"cannot parse value <ip>"`, where `<ip>` is the provided `NODE-IP`.

#### `JoinNetwork NET-ID`

Doesn't seem to do anything.

Output `OK` if `NET-ID` is between 1 and 99999; otherwise `FAIL`.

#### `LeaveNetwork NET-ID`

Doesn't seem to do anything.

Output `OK` if `NET-ID` is between 1 and 99999; otherwise `FAIL`.

#### `GetAncestors AMOUNT [BLOCK-HASH]`

Retrieves the hashes of `AMOUNT` blocks from the chain starting from a specific or the current best block. 

The output is sorted from newest to oldest block.

Bug: The node becomes unresponsive if `AMOUNT` is larger than the number of blocks in the chain.
This causes the client to hang.

Output format: JSON (list).

*Example*

```
[
    "b5d2f5a697ad5da066c60091c79fb060c89f60acfd2d39f5bc74906151fbd4fa",
    "6df593bc9e2027fb435e5d81ac17900e9d83cff850bc247e09a78eec53a766df",
    "c0cf70665dd379c10f42ab30018c5e5e7d816b9b8381df08c4726aece5d5dbd7"
]
```

#### `GetBranches`

Retrieves the full tree structure from the last finalized block onwards.

Output format: JSON (object).

*Example*

```
$ simple-client GetBranches
{
    "children": [
        {
            "children": [
                {
                    "children": [],
                    "blockHash": "c0cf70665dd379c10f42ab30018c5e5e7d816b9b8381df08c4726aece5d5dbd7"
                }
            ],
            "blockHash": "45d6cdbc49bf4138306248c60c363a301547fff68f9c2839dd5ccb5b490c52c3"
        }
    ],
    "blockHash": "b46f0642e1a945cda7ddca0014329373277aa8ea389bbfd6ed267c38ee995d5e"
}
```

#### `GetBannedPeers`

Output format: `Either` value marshalled directly from Haskell.

Output: `Right {}`.

#### `Shutdown`

Doesn't seem to do anything.

Output: `OK`.

#### `TpsTest`

Output format: Haskell `Either` value.

Output: `Left "gRPC response error: Feature not activated"`.

#### `DumpStart`

Output format: Text.

Output: `gRPC response error: Feature not activated`.

#### `DumpStop`

Output format: Text.

Output: `gRPC response error: Feature not activated`.

#### `MakeBaker BAKER-KEYS ACCOUNT-KEYS`

Generates the transaction data necessary to become a baker.
