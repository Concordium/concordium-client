# Simple Client

## Requisites

For generating the protobuf files we need to install [protoc](https://github.com/google/proto-lens/blob/master/docs/installing-protoc.md).

Executing `stack build` should now succeed.

Don't forget to run `git submodule update --init --recursive` before `stack build`.

## Usage

The binary has several modes of operation that can be specified by the first argument:

- `LoadModule <source.acorn>` loads an Acorn source file into the local context. Examples of Acorn modules can be seen in `test/contracts/`.
- `ListModules` shows the list with the modules stored in the local context
- `SendTransaction <transaction.json> --grpc-ip <ip> --grpc-port <port>` takes a transaction in JSON format, signs it and sends it to the specified gRPC server. Examples of transactions can be seen in `test/transactions/`. If the flag `-h/--hook` is provided, it will install a hook for the transaction in the node, in order to query it later.
- `HookTransaction <transaction-hash> --grpc-ip <ip> --grpc-port <port>` queries the node for the information about the execution of a transaction.

The supported transaction types are:
* [x] `DeployModule`
* [x] `InitContract`
* [x] `Update`
* [x] `Transfer`
* [x] `DeployCredential`
* [x] `DeployEncryptionKey`
* [x] `AddBaker`
* [x] `RemoveBaker`
* [x] `UpdateBakerAccount`
* [x] `UpdateBakerSignKey`
* [x] `DelegateStake`

Queries to the state are also available:
* [x] `GetConsensusInfo`
* [x] `GetBlockInfo`
* [x] `GetAccountList`
* [x] `GetInstances`
* [x] `GetAccountInfo`
* [x] `GetInstanceInfo`
* [x] `GetRewardStatus`
* [x] `GetBirkParameters`
* [x] `GetModuleList`
* [x] `GetModuleSource`

For a reference on this queries, check the [Wiki](https://gitlab.com/Concordium/notes-wiki/wikis/Consensus-queries#state-queries)
