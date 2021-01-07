# Transaction generator

The transaction generator has two modes, it can send simple transfers or
encrypted transfers with the prescribed transactions per second rate.

## Caveats

The generator assumes that it is the only party sending from the account.
If this is violated then the generator will fail with incorrect nonce at some point during its execution.

## Simple transfers

Use

```console
tx-generator -- --keyPair $ACCOUNT --tps 10 --log --addresses accounts.json --grpc-port 10002 --grpc-ip localhost
```

where

- `--tps` specifies the transactions per second rate
- `--log` is optional, but if given the generator will emit one line per transaction that it sends
- `--addresses` is optional. If present it should be a JSON array of account addresses, for example the output of
   `concordium-client raw GetAccountList`
  If omitted then all transfers will be self transfers.
- `--grpc-port`, `--grpc-ip` have the same meaning as in `concordium-client`, with the same defaults.
- `--keyPair` is required and is the account from which the transactions will be sent.
  This expected format is the same as that output from the `crypto/rust-bins/src/bin/client.rs` tool, i.e., genesis account format.

## Encrypted transfers

Use

```console
tx-generator -- --keyPair $ACCOUNT --tps 10 --log --addresses accounts.json --grpc-port 10002 --grpc-ip localhost --encrypted
```

The differences from the simple transfer are

- `--addresses` is a mandatory field
- the account the transfers are being sent from should not receive any encrypted transfers. If this is not observed then transfers will become
  invalid at some point, leading to skewed test results if, e.g., TPS is being measured.
