# Pixura Contracts

Smart Contracts and various language bindings for SuperRare and Pixura ecosystem.

## Tests

Tests require `docker` and `docker-compose`. To run:

```bash
make contract-tests
``` 

## SDKs

### Haskell 

Haskell bindings for the pixura smart contracts.

These can be easily imported by adding this project to your stack.yaml.

### PureScript 

Purescript bindings for the contracts can be found in the `purs-contracts` directory.


The `v4` and `v5` directories indicate which solc compiler was used. From a usage perspective there is no difference.


### Javascript/Typescript

WIP, eventually compiled from the PureScript libraries.
