# Changelog for concordium-client

## Unreleased

- Subcommand `account show` now displays protocol level tokens info.
- Subcommand `raw GetAccountInfo` now displays protocol level tokens info.
- Add `transaction transfer-plt` command to transfer protocol level tokens from one account to another.
- Add `raw GetTokenList` command to display the list of protocol level tokens.
- Subcommand `raw GetNextUpdateSequenceNumbers` now displays the next update sequence number for protocol level tokens chain updates.
- The `consensus chain-update` command supports creating protocol level tokens now.

## 8.1.0

- The `transaction status` command output includes the `parameter` for
  `ContractInitialized` events.
- From protocol version 8, raw command `GetPoolStatus` indicates if a validator is suspended and,
  if it is in the current committee, if it is primed for suspension and the current count of
  missed rounds.
- From protocol version 8 `account show` also shows if a validator is suspended.
- Add command `pool status` for getting information about a pool.

## 8.0.0

Note: due to API changes, this release may not work correctly with node versions prior to 8.

- Support node version 8 and protocol version 8.
- Support for suspend/resume in validator update transactions.
- Add command `consensus detailed-status` for getting detailed consensus status (from protocol
  version 6).
- Add `raw GetConsensusDetailedStatus` that presents the detailed consensus status as JSON.
- Update GHC version to 9.6.6 (lts-22.39).
- Add raw commands `GetScheduledReleaseAccounts`, `GetCooldownAccounts`,
  `GetPreCooldownAccounts` and `GetPrePreCooldownAccounts` for querying
  accounts with scheduled releases, cooldowns, pre-cooldowns and pre-pre-cooldowns.
- Raw commands `GetBlockTransactionEvents` and `GetTransactionStatus` include the `parameter`
  for `ContractInitialized` events.

## 7.0.1

- Display the correct "at disposal" balance when used with node versions prior to 7.
- Fix a bug in correctly accounting for parsed events.
- Change `--out` flag for `./client validator add` to
  `--validator-credentials-out`, fixing an issue where this command had two
  conflicting`--out` options.

## 7.0.0

- Support node version 7 and protocol version 7.
  - Display account balance "at disposal". (Note, this will (incorrectly) show as 0 if connecting to
    an older version of the node.)
  - List the cooldowns on an account, and their expiration times.
- Improved checks when configuring a validator or delegator.
- Fix the display of the expected expiry of pending changes to an account's stake, so that they
  correctly account for the change taking place at a payday.

## 6.3.0

- Remove command `raw SendTransaction`.
- Remove command `transaction send-shielded` to disable the transfer of CCD from the shielded
  balance of the account to the shielded balance of another account.
- Remove command `account shield` to disable the transfer of CCD from the public balance to the
  shielded balance of an account.
- Add command `transaction add-signature` to add a signature to a partially-signed transaction.
- Revise command `transaction submit` to submit already-signed transactions to the chain.
  (This is a breaking change, as transactions must now already be signed, e.g. with
  `transaction add-signature`.)
- Add optional `--out` flag to all transaction-creating commands to output a partially-singed
  transaction to a file.
- Update GHC version to 9.6.4 (lts-22.9).
- Update Rust version to 1.73.
- Preliminary support for protocol version 7.

## 6.2.1

- Remove uses of `baker` term when printing chain parameters.
- Remove remaining uses of `baker` in output of `validator add` and `validator
  configure` command and in their options.

## 6.2.0

- Revise client's reconnect handling so that the client will no longer attempt
  to automatically reconnect on timeouts and node resource exhaustion.
- Rename bakers to validators in output.
- Add additional configuration options that use `validator` in place of `baker`.
  For example `concordium-client validator add`.
  The older options still exist, but are hidden.
- The `module inspect` command now attempts to print any embedded verifiable
  build information.
- The `module deploy` command now warns if a module is being deployed that does
  not have embedded verifiable build information.

## 6.1.0

- Add `baker win-time` command for determining the earliest time a specified baker is expected to
  bake.
- End stream consumption early if an error is returned.
- Add support for the following node version 6.1 queries under the `raw` command:
  - `GetBakersRewardPeriod`
  - `GetBlockCertificates`
  - `GetBakerEarliestWinTime`
  - `GetWinningBakersEpoch`
  - `GetFirstBlockEpoch`
  - Add support for `CommissionRates` in `CurrentPaydayBakerPoolStatus` (Only available for node versions > 6.0).
  - Show all options for importing an account.

## 6.0.1

- Add an unit (ms) to the minimum block time shown in the chain parameters for P6.

## 6.0.0

- Remove a stray `CTrue` in output of `consensus show-chain-parameters`.
- The client now outputs protocol version as part of raw `GetBlockInfo` and
  `block show` commands.
- The client needs node version at least 5.4.
- Add `raw GetNextUpdateSequenceNumbers` subcommand.
- Add node version to the output of `raw GetNodeInfo`.
- Add current timeout duration, current round, current epoch and trigger block time to the output
  of `raw GetConsensusInfo`.
- Add current timeout duration, current round, current epoch and trigger block time to the output
  of `consensus status` when they are present.
- Add round and epoch to the output of `raw GetBlockInfo`.
- Add round and epoch to the output of `block show` when they are present.
- Print "Block time" instead of "Slot time" in the output of `block show`.
- In the output of `consensus show-parameters`, only print election difficulty when present.

## 5.2.0

- Fix a bug that caused another code-leftover to be displayed in the finalization proof
  line of the gas rewards section of the `consensus show-chain-parameters` output. This
  is only relevant when the queried chain runs protocol version 6.
- Add `raw GetBlockTransactionEvents` that prints the list of transaction outcomes
  in a given block.

## 5.1.1

- Fix a bug that caused commission-rate related reject reasons to be incorrectly
  displayed in the output of `baker configure` and `transaction status`.
- Fix a bug that caused leftover code to be displayed in the finalization proof
  line of the gas rewards section of the `consensus show-chain-parameters` output.
- Fix a bug that caused the output of `raw GetCryptographicParameters` to be printed
  in a non-versioned context.
- The help-message for `raw Shutdown` now properly describes its effects.

## 5.1.0

- The `--grpc-authentication-token` option has been removed.
- The client now uses the V2 GRPC API exposed by the node. This introduces some breaking
  changes:
    - Since the node serves the V2 GRPC API at port 20000 by default, the default value of
      the `--grpc-port` option has been updated to reflect this.
    - `raw` family of commands:
        - Commands `raw GetTransactionStatusInBlock`, `raw StartBaker`, `raw StopBaker`,
          `raw JoinNetwork` and ` raw LeaveNetwork` have been removed.
        - Command `raw GetBlockSummary` has been removed, and replaced by the commands
          `raw GetBlockPendingUpdates`, `raw GetBlockSpecialEvents`,
          `raw GetBlockChainParameters` `raw GetBlockFinalizationSummary`. These provide
          a more granular way of accessing to the same data.
        - `raw BanNode` and `raw UnbanNode` no longer support node IDs, but now rather
          take just an IP address.
        - `raw DumpStart` takes a parameter specifying the path of the file to write
          dumped packets to, and furthermore supports a flag to specify whether raw
          packets should be written to the file.
        - Output of `raw GetBannedPeers` prints a JSON list of banned IP addresses,
          represented as strings.
        - Output of `raw GetPeerUptime` now prints an integer representing the uptime
          of the node in milliseconds.
        - Slight changes to `raw GetNodeInfo`. Notably the baker ID is now included in
          the output when the node is in the baker or finalization committee. Various
          consensus-related details about the node is also elaborated upon.
        - Slight changes to `raw GetPeerData` output. Notably the catch-up status and
          consensus-related details about the peer is elaborated upon.
        - `raw SendTransaction` no longer allows for specifying a network ID.
    - `consensus show-parameters` now additionally prints the election difficulty.
    - Slight changes to error message information and their phrasing for other families
      of commands.

## 5.0.2

- Receive function parameters are now displayed as JSON in `transaction status`
  whenever they could be succesfully parsed by a smart contract schema embedded
  in the module or supplied by the user using the `--schema` option.

## 5.0.1

- Add support of contract schema V3.
	  - V3 schemas offer the same options as V2, but also optionally includes a schema for contract events.
	  - `transaction status` now displays contract events, and a schema can be provided with `--schema`, which
	    will be used to parse the contract events. By default events are parsed with the schema embedded in the
      contract, if present.
	  - This enables concordium-client to interact with contracts and schemas
	    using `concordium-std` version 5.
- Improved formatting of `transaction status` output using contract schemas if
  they are available for displaying contract events.
- Output function parameters as hex strings in `transaction status`.

## 5.0.0

- Add support for protocol version 5.
- Add a `--secure` flag to enable connecting to gRPC using TLS.
  All commands that query the node support this.

## 4.2.0

- Fix handling of `--no-confirm` in `contract init`, `contract update`, `module
  deploy`, and `register data` transactions. This flag is now respected.
- Add support for importing accounts exported by the browser extension wallet.
- Make the output of CCD amounts consistent when printing transaction outcomes.
  The output should now always `x.y CCD` for some `x` and `y`.

## 4.1.0

- Fix bug in contract schema parsing caused by endiannes confusion.
- Add support for smart contract schema V2.
  - V2 schemas offer the same options as V1, but can also include a schema for
    the error type.
  - This enables concordium-client to interact with contracts and schemas
    using `concordium-std` version 4.

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
