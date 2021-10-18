# Concepts

#### Smart contracts

Smart contracts consist of:
* Some amount of GTUs (contained in value `amount`).
* Current state (contained in value `model`).
* Implementation of the functions `init` and `receive`.

The `init` and `receive` functions are invoked when the contract is initialized and "invoked", respectively.
Both functions may update `model` and/or perform transactions to change `amount`.

The functions of the smart contract are defined in a Rust module.
Such a module may define multiple contracts and also types and other functions used by the contracts.

Smart contracts are deployed and instantiated (with some initial state) using the special transaction types
`DeployModule` and `InitContract`.

#### Nonce

All accounts have a "nonce" counter (starting from 1) which is incremented for each successfully committed transaction sent from the account.

All transactions must include the current nonce value for the sending account to ensure that transactions
are ordered and that two otherwise identical transactions can be distinguished (i.e. it guarantees that the transactions have different hashes).

The current nonce for account `ACCOUNT-ID` can be retrieved using the command `GetAccountInfo ACCOUNT-ID` (grep for `accountNonce`).

If a transaction with a reused nonce is submitted, the following helpful error message is returned:
```
concordium-client: user error (gRPC response error: Got non-success response from FFI interface Stale)
```

The nonce must be provided if a transaction is created from a (JSON) payload file (i.e. `transaction submit` or `SendTransaction`).
For `transaction send-gtu`, the nonce is resolved from the backend by default.

#### "Best" block

Commands that operate on a specific block default to the "best" block if the parameter is omitted.

There is a bit of a race condition in the way this best block is queried:
To get the best block, we make a call, and then we need a separate call to get the block info.
In the meantime the best block could have in fact been pruned due to finalization.

## Configuration

Accounts and keys may be stored in config files on disk to avoid having to pass it as command line options.
The config directory may be set using the `--config PATH` option and defaults to `$XDG_CONFIG_HOME/concordium`.
The variable `XDG_CONFIG_HOME` is defined by the
[XDG standard](https://hackage.haskell.org/package/directory-1.3.6.0/docs/System-Directory.html#v:XdgConfig)
as the location of user specific configuration.
If not set or empty, it has the following system-dependent defaults:

* Unix: `$HOME/.config`
* Windows: `%APPDATA%` (`C:\Users\<user>\AppData\Roaming`)

The expected structure inside the config directory is

```
<config-dir>
  /concordium
    /accounts
      /names.map    # Mapping from account name to addresses (optional)
      /<address>
        /<n>.sign   # n'th sign key for account <address>
        /<n>.verify # n'th verify key for account <address>
```

The account name to address map `accounts/names.map` is a key-value file formatted as
```
<account1_name> = <account1_addr>
<account2_name> = <account2_addr>
...
```

An account's name as specified in this map may be used in place of its address in the "sender" option of transaction commands.
The tool will then look up and use the address of the account with that name.
If no account is provided, it defaults to the one with name "default".

Note that the account map is only consulted once and only if the account reference is not a valid address.
So the map cannot be used to map the address nor name of one account to another.

The key files (named by `<n>` above) must exist in pairs with numeric names and the extensions shown above.
Account names may consist of letters, numbers, and the symbols '-' and '_'.

### Initialization

The adhoc script `scripts/init-config.sh <address>` initializes this config structure with `<address>` as the default account.
After confirming the directory to be initialized (which includes nuking it),
the script expects a JSON string in the following format to be pasted into stdout.

```
{
    <n>: {
        "signKey": <sign-key>,
        "verifyKey": <verify-key>
    },
    ...
}
```

This is the same format as that expected by the `--keys` flag of the [`transaction send-gtu`](#transaction-send-gtu-flags) command.
The script is a temporary solution and will be replaced with proper commands.

*Example*

```
$ scripts/init-config.sh 4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y
Press ENTER to confirm writing config to /home/testuser/.config/concordium (or Ctrl-C to cancel):
Writing '/home/testuser/.config/concordium/accounts/names.map'.
Input keys in the same JSON format as the one expected by the --keys flag.
{
        "0": {
            "signKey": "b2217dd42238d5c16d2f3308ceefed96c1316d5f70d682877f2480fdbb15afe9",
            "verifyKey": "414518b56ae0bbef0802c39eb80b5dccbf7bd557779303a8a467ab3f7d5ac07a"
        },
        "1": {
            "signKey": "8097175288884c429428bf828462d1a29a7845b005c248bb7f2c709b6afbffb3",
            "verifyKey": "0ce48959ee09e8d5c178f718f876731b6164f39ff23865e134bb86a41b893664"
        },
        "2": {
            "signKey": "10a117dde270c9c38921602290f36f8863653db807881b9b09c4cea578a0a22a",
            "verifyKey": "3c3a341f906a9cb6a5e204d216df34d2d540001762dfe802ddef29972cfbd150"
        }
    }
^D
Writing '/home/testuser/.config/concordium/accounts/4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y/0.sign'.
Writing '/home/testuser/.config/concordium/accounts/4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y/0.verify'.
Writing '/home/testuser/.config/concordium/accounts/4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y/1.sign'.
Writing '/home/testuser/.config/concordium/accounts/4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y/1.verify'.
Writing '/home/testuser/.config/concordium/accounts/4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y/2.sign'.
Writing '/home/testuser/.config/concordium/accounts/4DY7Kq5vXsNDhEAnj969Fd86g9egi1Htq3YmL2qAU9cXWj2a1y/2.verify'.
```




