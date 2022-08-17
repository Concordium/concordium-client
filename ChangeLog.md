# Changelog for concordium-client

## Unreleased

- Fix bug in contract schema parsing caused by endiannes confusion.

## 4.0.4

- Add support for contract schemas containing the version.
- Add support for the contract schema types, ULeb128, ILeb128, ByteList and ByteArray.

## 4.0.3

- Fix calculation of payload size of a configure baker transaction in
  the function `bakerConfigurePayloadSize` in Transaction.hs used by
  the wallet-proxy.
- Fix help text of `baker add` and `baker configure` commands that referred to
  `raw GetChainParameters`.

## 4.0.2

- Add support for v1 smart contracts.
- Add `contract invoke` command for simulating contracts locally on the node.
- Make `module deploy` expect modules with a version prefix.
  - This prefix is automatically added when building with cargo-concordium
    version >= 2.
  - Add the flag `--contract-version` to support modules without the version
    prefix.
- `contract update` command now uses `--entrypoint` to specify the function to
  invoke. This is renamed from the previous `--func`.
- When calling `contract update` or `contract invoke` with a non-existent
  entrypoint the fallback entrypoint is called if one specified in the contract.
- Related to delegation:
  - Add commands `delegator add`, `delegator configure` and `delegator remove`.
  - Add commands `baker configure` , `baker update-url` and `baker update-delegation-status`.
  - Update existing commands `baker add`, `baker remove`, `baker set-key`, `baker update-restake`
    and `baker update-stake` so that in Protocol version < 4, they generate the former P3
    transaction, and in Protocol version 4, they generate the relevant "configure baker" transaction.
  - Support raw queries `GetPoolStatus` and `GetBakerList`.
- Add `consensus show-chain-parameters` subcommand to print the chain parameters.

## 3.0.4

- Rename `--encrypted` and `--decrypt-encrypted` flags to `account show` to
  `--shielded` and `--reveal-shielded`.

## 3.0.3

- support the same range of tag names for LEI as is supported by identity
  issuance command line tools. Previously only UNNAMED#13 was supported and now
  both lei and the former are.

## 3.0.2

- credentials revealing newly introduced attribute LEI can be deployed

## 3.0.1

- rename GTU token to CCD
- rename send-gtu, send-gtu-scheduled and send-gtu-encrypted to send,
  send-scheduled and send-shielded.
- rename account encrypt/decrypt to account shield/unshield

## 3.0.0

- show line breaks, tabs etc. in memo transfers (when it's CBOR encoded string), instead of escaping them
  as `\n`, `\t` etc.
- Display memo as JSON in a more readable way.
- Add time units to slot duration and epoch duration in consensus status.
- Update `register-data` command to register data as CBOR encoded strings or JSON using the new flags
  `--string` and `--json`. Raw data can still be registered using the new flag `--raw`.
- Add `raw DisconnectPeer`, a dual to the existing `raw ConnectPeer`.
- Warn user when trying to add a baker with stake below the minimum threshold.
- Improve how contract schemas are shown as JSON.
  - Display complex types in arrays correctly.
  - Use angle brackets to indicate placeholders, e.g. `"<UInt16>"` instead of `"UInt16"`.
- Improve `module inspect`:
  - Show all contracts from a module regardless of whether a schema is included
  or not.
  - Show the receive methods for contracts as well.
- Allow sending transactions where the sender is an account alias.
- Add command for generating aliases of an address.

## 1.1.1

- show smart contract state as raw bytes, if schema is provided but doesn't include the state type.
- warn about sending transfers with oversized memos.
- show amount in GTU instead of µGTU when trying to send an encrypted amount that is larger than
  the encrypted balance.

## 1.1.0

- The `account show` command can receive a credential registration ID instead of a name or address.
- support sending the three new transaction types, i.e. TransferWithMemo, EncryptedTransferWithMemo
  and TransferWithScheduleAndMemo
- show transfer memo in transaction status
- show protocolVersion, genesisIndex, currentEraGenesisBlock and currentEraGenesisTime in
  consensus status
- this version is only compatible with node version 1.1.0 and later.
- warn the user when trying to deploy a wasm module of size > 65536 bytes
- warn the user when adding a baker staking more than 95% of the amount on the account
- correct printing of cooldown time in `account show`
- The `raw GetBlocksAtHeight` command can take an optional genesis index, given by the
  `--genesis-index <GENINDEX>` option.  If supplied, the height is considered relative to the
  genesis block at this index. Additionally, the `--restrict` flag can be specified to only return
  blocks that belong to the specified genesis index (or index 0, if the genesis index was not
  specified).
- show the genesis index and relative block height in `block show`

## 1.0.1

- support the new mobile wallet export format. This breaks compatibility with
  the old format.

## 1.0.0

- baker generate-keys outputs public and private keys in separate files, and
  optionally includes baker-id
- fix printing of "expiry too soon warning" when the expiry is less than 30s
  from now.
- produce an error when an invalid credential holder or invalid key is
  specified in the 'signers' option of the 'send-gtu' command.
- Improve error message for SC update failures.
- Fix incorrect parsing of `ReceiveName` when the contract field contains a dot.

## 0.7.0

- Add ContractName and ReceiveName to schema.
- Add UInt128 and Int128 to schema type.
- Add 'transaction register-data' command.
