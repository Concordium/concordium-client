
### Checkout & setup data

- Get latest of `crypto`'s `origin/master` branch
- Copy [p2p-client's identity_provider-5](https://gitlab.com/Concordium/p2p-client/blob/develop/scripts/genesis-data/ip_private_keys/identity_provider-5.json) private data into `crypto/rust-src/crypto/rust-src/simple_id_client/database/identity_providers_public_private.json`
- Copy [p2p-client's global.json](https://gitlab.com/Concordium/p2p-client/blob/develop/scripts/genesis-data/global.json) into `crypto/rust-src/simple_id_client/database/global.json`

### Build & launch the id_server

- `cd crypto/rust-src/simple_id_client && cargo build --release`
- `../target/release/server` (server currently has no output when it starts successfully)

Leave this server running in one tab. The middleware relies on it for id/credential generation process.

### Build & launch the middleware

In a new tab, get the latest of the `http-server-interface` branch of `simple-client`.

```
cd simple-client
git fetch origin && git checkout http-server-interface && git reset --hard origin/http-server-interface
stack ghci
# Pick option 2
```

See the bottom of `middleware/Api.hs` for the `debugTestFullProvision` function.

### Generate identity + credentials request

You can boot `ghci` with the supported ENV vars:

```
NODE_URL="localhost:11100" ES_URL="http://localhost:9200" SIMPLEID_URL="http://localhost:8000" stack ghci
```

Then in GHCI you can do:

```
λ: :r
...
Ok, 16 modules loaded.
λ: import Api
λ: debugTestFullProvision
```

This will currently:

- Call `simple_id_server` to generate an ID
- Call `simple_id_server` to generate credentials for that ID
- Call gRPC with the crafted `DeployCredentials` transaction
- Call gRPC a `Transfer` for a GTU drop to the new account

Use `:r` to reload ghci again on further changes.

The last thing you should see if successful is something like:

```
✅ Requesting GTU Drop for: AddressAccount 1FBcjuG4XBaovzqRKwrfDidhjG1aHDZuzM
✅ got nonceQueryResponse, progressing...
Installing hook for transaction cfe6490e80cbccc6c0e33fc5f60d58c877b6315e93c4068dbbb7126d38df1139
{
    "status": "absent",
    "results": [],
    "expires": "2019-09-23T15:46:59.9327019Z",
    "transactionHash": "cfe6490e80cbccc6c0e33fc5f60d58c877b6315e93c4068dbbb7126d38df1139"
}
✅ Transaction sent to the baker and hooked: cfe6490e80cbccc6c0e33fc5f60d58c877b6315e93c4068dbbb7126d38df1139
TransactionJSON {metadata = TransactionJSONHeader {thSenderKey = 3e4f11b8a43f5b4c63f9d85ae9de365057c9bce8c57caf84e34f1040e5f59ecd, thNonce = Just (Nonce 2), thGasAmount = 4000}, payload = Transfer {toaddress = AddressAccount 1FBcjuG4XBaovzqRKwrfDidhjG1aHDZuzM, amount = 100}, signKey = b52f4ce89e78e45934851e395c6258f7240ce6902526c78a8960927c8959a363}
"Done."
```


### Following the transaction

The transaction is submitted with `hookIt = True` so currently I've been checking in another shell:

```
simple-client --grpc-ip=127.0.0.1 --grpc-port=11142 HookTransaction 649339ed1c0a0b19747543bf44a692ddd393d6a22b48f40c099c17b04afeaeaa
```

I keep re-running that query and watching logs until I get a resolution.
