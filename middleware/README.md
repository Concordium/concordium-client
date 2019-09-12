
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

Edit the following line to match your local environment/node:

```
let backend = COM.GRPC { host = "127.0.0.1", port = 11160, target = Nothing }
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

Use `:r` to reload ghci again on further changes.

The last thing you should see if successful is something like:

```
✅ Deployed account address will be: 5W3bEM996GaYzbuBX7WEdt9fmQeV
Installing hook for transaction 649339ed1c0a0b19747543bf44a692ddd393d6a22b48f40c099c17b04afeaeaa
{
    "status": "absent",
    "results": [],
    "expires": "2019-09-12T15:44:41.4848528Z",
    "transactionHash": "649339ed1c0a0b19747543bf44a692ddd393d6a22b48f40c099c17b04afeaeaa"
}
✅ Transaction sent to the baker and hooked: 649339ed1c0a0b19747543bf44a692ddd393d6a22b48f40c099c17b04afeaeaa
```


### Following the transaction

The transaction is submitted with `hookIt = True` so currently I've been checking in another shell:

```
simple-client --grpc-ip=127.0.0.1 --grpc-port=11142 HookTransaction 649339ed1c0a0b19747543bf44a692ddd393d6a22b48f40c099c17b04afeaeaa
```

I keep re-running that query and watching logs until I get a resolution.
