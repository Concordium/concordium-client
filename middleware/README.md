
The middleware provides a wrapper around Identity Issuance and some basic chain interaction, and provides some niceties like off-chain nonce tracking to allow for quick successive transactions without nonce collisions, and transaction tracking/querying for accounts.

It is currently POC grade software built for the Demo Wallet


### Building & launching the ID server

The middleware needs an instance of the ID Server to be running for the id/credential generation process. Here's how to set it up for the middleware:

#### Checkout & setup data

- Get latest of [crypto](https://gitlab.com/Concordium/crypto)'s `origin/master` branch
- Copy [p2p-client's identity_provider-5](https://gitlab.com/Concordium/genesis-data/blob/master/ip_private_keys/identity_provider-5.json) private data into `crypto/rust-src/crypto/rust-src/simple_id_client/database/identity_providers_public_private.json`.
  - It needs to be wrapped in `[]`
  - We use `5` just to offset away from the ones other folks might be using in testing
- Copy [p2p-client's global.json](https://gitlab.com/Concordium/genesis-data/blob/master/global.json) into `crypto/rust-src/simple_id_client/database/global.json`

Now you can build & boot the ID server:

- `cd crypto/rust-src/simple_id_client && cargo build --release`
- `../target/release/server` (server currently has no output when it starts successfully)

Leave this server running in one tab.


### Stack flags

:warning: The middleware exe is flagged out of the build by default. You can enable it for build/install like so:

```
stack build  --flag "simple-client:middleware"
stack install simple-client:middleware
```

### Build & launch the middleware

#### Scenario: Against local testnet

In a new tab, open up the `simple-client` repo (this repo), and run this helper script:

```
./scripts/bootMiddleware.sh
```

This will
- Check Docker for a locally running baker node and grab it's port
- Assume the ID server is running on its default of 8000
- Assum the Docker ElasticSearch image is running on its default of 9200


### Scenario: Manual/Production

After a successful `stack install`, assuming `middleware` is now on your PATH, the following ENV vars are required:

```
NODE_URL="localhost:$NODE" \
ES_URL="http://localhost:9200" \
SIMPLEID_URL="http://localhost:8000" \
middleware
```


### Scenario: Development testing

See the bottom of `middleware/Api.hs` for the `debugTestFullProvision` function.

To run it:

```
cd simple-client
stack ghci --flag "simple-client:middleware"
# Pick middleware option
```

You can boot `ghci` with the supported ENV vars:

```
NODE_URL="localhost:11100" \
ES_URL="http://localhost:9200" \
SIMPLEID_URL="http://localhost:8000" \
stack ghci --flag "simple-client:middleware"
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
- Bump the next nonce using ElasticSearch*
- Call gRPC with a `DeployCredentials` transaction
- Call gRPC with a `Transfer` for a GTU drop to the new account
- Write the bare transaction into ElasticSearch

<sub>&#42;This allows multiple transactions to be submitted in quick succession without colliding nonces</sub>


Use `:r` to reload ghci again on further changes.

The last thing you should see if successful is something like:

```
✅ Requesting GTU Drop for AddressAccount 3uVGUijtdD5JLJmmf3Q4pqe1y4H3PHfW469GphtprZuTfTRxex
✅ got nonceQueryResponse, progressing...
Installing hook for transaction 5dcaf8d6d204b54988328d1bf6899690d185d6354a261b82a30402b7da5e2a4b
{
    "status": "absent",
    "results": [],
    "expires": "2020-01-31T16:53:14.0571333Z",
    "transactionHash": "5dcaf8d6d204b54988328d1bf6899690d185d6354a261b82a30402b7da5e2a4b"
}
✅ Transaction sent to the baker and hooked: 5dcaf8d6d204b54988328d1bf6899690d185d6354a261b82a30402b7da5e2a4b
TransactionJSON {metadata ...
... etc
```


#### Following a transaction

All middleware transactions are submitted with `hookIt = True` so you can manually check it in another shell if you wish:

```
simple-client --grpc-ip=127.0.0.1 --grpc-port=11142 HookTransaction 649339ed1c0a0b19747543bf44a692ddd393d6a22b48f40c099c17b04afeaeaa
```

You can keep re-running that query and watching logs until there is a resolution.
