# Changelog for concordium-client

## Unreleased changes

- baker generate-keys outputs public and private keys in separate files, and
  optionally includes baker-id
- fix printing of "expiry too soon warning" when the expiry is less than 30s
  from now.

## 0.7.0
- Add ContractName and ReceiveName to schema.
- Add UInt128 and Int128 to schema type.
- Add 'transaction register-data' command.
