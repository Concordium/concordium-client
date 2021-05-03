# Interaction with Smart Contracts

## Basic Workflow
1. **Write a smart contract in Rust and [compile it to Wasm.](https://gitlab.com/Concordium/smart-contracts#compiling-smart-contracts-to-wasm)**
   - Make sure to use `wasm-strip` on the `.wasm` file as described.
2. **Deploy the smart contract module on the chain:**
```
concordium-client module deploy <module-name>.wasm [--name <name>]
```
  - If successful, the module reference is printed, which should be used in next step.
  - The `--name` flag can be used to give the module a name that is easy to remember.
3. **Initialise an instance of the contract:**
```
concordium-client contract init <module-tbd> --energy <max-energy> [--func <init-name>] [--params <binary-file>] [--path] [--name <name>]
```
  - The `<module-tbd>` should be __one__ of three things:
    - if the `--path` flag is used:
      - the path to a module file (of which the reference then is calculated using hashing)
    - otherwise:
      - a module reference,
      - or a module name (fx specified with the `--name` during module deployment)
  - The maximum energy should be specified manually with the `--energy` flag.
  - By default the `"init"` function is used, but a different init function can be specified using the `--func` flag.
  - If the init function takes parameters, they should be specified in a binary file with little endian encoding and provided
      with the `--params` flag.
    - Simple parameter files can be create by hand, fx a `u64` with value `1`: `printf "\x01\x00\x00\x00\x00\x00\x00\x00" > <binary_file>`
  - The `--name` flag can be used to give the contract a name that is easy to remember.
4. **View the state of an contract:**
```
concordium-client contract show <index-or-name> [--subindex <address-subindex>]
```
  - `<index-or-name>` can be either an index or a contract name.
  - Currently, we just use the index, so the subindex defaults to `0`.
5. **Update a contract, i.e. call a receive function:**
```
concordium-client update <index-or-name> --energy <max-energy> [--func <receive-name>] [--params <binary-file>] [--subindex <address-subindex>]
```
  - `<index-or-name>` can be either an index or a contract name.
  - Similar to `init`, it takes an optional `--func` flag, which defaults to `"receive"`.
  - Parameters work in the same way as with `init`.

## Additional Commands
### For Modules
**List all modules on chain:**
```
concordium-client module list
```
**Get binary source of module:**
```
concordium-client show <reference-or-name> --out <out-file>
```
  - `<reference-or-name>` should be either a module reference or a module name.
  - `<out-file>` is the file to output the source code to (use `-` for stdout).

**Add a local name to a module:**
```
concordium-client module name <module-or-ref> --name <name> 
```
  - `<module-or-ref>` should be a module reference or a path to the module file.
### For Contracts
**List all contracts (instances) on chain:**
```
concordium-client contract list
```
**Add a local name to a contract:**
```
concordium-client contract name <index> --name <name> [--subindex <address-subindex>] 
```
