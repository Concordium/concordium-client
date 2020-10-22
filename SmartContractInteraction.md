# Interaction with Smart Contracts

## Basic Workflow
1. **Write a smart contract in Rust and [compile it to Wasm.](https://gitlab.com/Concordium/smart-contracts#compiling-smart-contracts-to-wasm)**
   - Make sure to use `wasm-strip` on the `.wasm` file as described.
2. **Deploy the smart contract module on the chain:**
```
concordium-client module deploy <module-name>.wasm
```
  - If successful, the module reference is printed, which should be used in next step.
3. **Initialise an instance of the contract:**
```
concordium-client contract init <module-ref-or-file>.wasm --energy <max-energy> [--func <init-name>] [--params <binary-file>]
```
  - The `<module-ref-or-file>` should be either a module reference OR a path to the module file (of which the reference then is calculated using hashing).
  - The maximum energy should be specified manually with the `--energy` flag.
  - By default the `"init"` function is used, but a different init function can be specified using the `--func` flag.
  - If the init function takes parameter, they should be specified in a binary file with little endian encoding and provided
      with the `--params` flag.
    - Simple parameter files can be create by hand, fx a `u64` with value `1`: `printf "\x01\x00\x00\x00\x00\x00\x00\x00" > <binary_file>`
4. **View the state of an contract:**
```
concordium-client contract show <address-index> [--subindex <address-subindex>]
```

  - Currently, we just use the index, so the subindex defaults to `0`.
5. **Update a contract, i.e. call a receive function:**
```
concordium-client update <address-index> --energy <max-energy> [--func <receive-name>] [--subindex <address-subindex>]
```
  - Similar to `init`, it takes an optional `--func` flag, which defaults to `"receive"`.

## Additional Commands
**List all modules on chain:**
```
concordium-client module list
```
**Get binary source of module:**
```
concordium-client <module-reference> --out <out-file>
```
  - The `<module-reference>` is printed when a contract is deployed, but can also be found using `concordium-client module list`.
  - `<out-file>` is the file to output the source code to (use `-` for stdout).

**List all contracts (instances) on chain:**
```
concordium-client contract list
```
