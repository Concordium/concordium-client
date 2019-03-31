- Install python3 grpc dependencies (see
  https://grpc.io/docs/tutorials/basic/python.html)

- Install base58 python library (pip3 install base58 should work)

- Make sure to `git submodule update --init`

- If on linux do
  - ./build-deps.sh
  - ./stack build

- If on other OSs do something equivalent to what those scripts do.

- After those steps you should have everything set up.

- You should now start a baker, e.g., use the `docker-compose` method in
  scripts/local of the p2p-client repository.
  
- You can now query the baker using `client.py`. `client.py` uses the
  haskell-built binary built by `stack build`. In order to run it you might have
  to modify the `callHaskell` method in `client.py` to reflect how the binary
  should be called. The default is to just run `./stack run`.
  
  You should look into `client.py` to see what commands are supported by the
  script, I only list a few here.
  
  - `$ ./client.py GetAccountList` will print the list of accounts on the last
    final block. By default the baker is queried at `localhost:11100` but you
    can modify that with `--baker ...` flag.
    
  - `$ ./client.py GetInstanceList` does the same as above but for instances.
  
  - `$ ./client.py GetInstanceState --index n --version m` will print the
    current local state of the instance with index `n` and version `m`, if it
    exists. `n` and `m` should be non-negative integers.
    
  - `$ ./client.py GetAccountState --account ADDR` will print the state of
    account `ADDR`. `ADDR` should be a base58 (bitcoin flavor) encoding of the
    address (as returned by `GetAccountList`).
    
  - `$ ./client.py SendTransaction --source FILENAME` will parse a transaction
    from `FILENAME` and send it. The transaction should be written in a JSON
    format as used in acorn tests. See `test/transactions` repository for
    examples of how to write transactions.
    
  - `$ ./client.py LoadModule --source FILENAME` will try to parse and process
    an acorn module and store it in a local cache. It does not talk to the
    baker. A module can be deployed by writing a "DeployModule" transaction
    after the module has been loaded into the local cache.
    
    
  
