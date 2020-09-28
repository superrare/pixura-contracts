# Pixura Contracts

Smart Contracts and various language bindings for the [SuperRare](https://superrare.co/activity) and [Pixura](http://platform.pixura.io/) ecosystem.

We use [chanterelle](https://docs.chntrl.io/en/latest/) to manage smart contract compilation, deployment, and testing.

## Init

```bash
make init
```

## Build

### Solidity

```bash
make compile-contracts
```

### Haskell

```bash
make hs-build
```

### PureScript

```bash
make purs-build-all
```

## Tests

Tests require `docker` and `docker-compose` and require the purescript libraries to be built. See [build](#build) for details. To run:

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

WIP, eventually to be compiled from the PureScript libraries.
