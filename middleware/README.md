The middleware is a proxy that supports the network dashboard. It essentially
exposes a REST http interface usable from a browser instead of the node's GRPC
interface.

The middleware exposes forwards the following endpoints
- GET `v1/consensusStatus`
- GET `v1/blockSummary/$blockHash`
- GET `v1/blockInfo/$blockHash`
- GET `v1/blocksByHeight/$height`
- GET `v1/transactionStatus/$transactionHash`

## Build

To build run `stack build`. 

## Run

The middleware needs access to the node and supports the following environment
variables

- `PORT` (defaults to 8081) the port at which the server will listen for
  incoming requests
- `NODE_URL` (defaults to `localhost:11100`) the address and port of the node's
  GRPC interface
- `RPC_PASSWORD` (defaults to `rpcadmin`) the token to access to node's GRPC interface.

The middleware can be run either with `stack exec middleware` after it is built,
or `stack run middleware` which will build and run it.
