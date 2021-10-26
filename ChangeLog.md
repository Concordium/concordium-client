# Changelog for concordium-client

## Unreleased changes

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
- Improve `module inspect` so it shows all contracts from a module regardless of
whether a schema is included or not. 

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
