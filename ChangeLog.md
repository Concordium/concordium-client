# Changelog for concordium-client

## Unreleased changes
- The `account show` command can receive a credential registration ID instead of a name or address.

## 1.1
- support sending the three new transaction types, i.e. TransferWithMemo, EncryptedTransferWithMemo
  and TransferWithScheduleAndMemo
- show transfer memo in transaction status
- show protocolVersion, genesisIndex, currentEraGenesisBlock and currentEraGenesisTime in 
  consensus status

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
