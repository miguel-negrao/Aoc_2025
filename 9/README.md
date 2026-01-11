Template for haskell project using cabal

Current version of GHC is 9.8.2

When changing ghc version, adjust cabal.project, name.cabal (the base version bound using https://wiki.haskell.org/Base_package) and .devcontainer.json

## Get part 2 output

```sh
cabal run
```

## Test part 1 and 2 examples

```sh
cabal test
```

## Bench part 1 and 2 

```sh
cabal bench
```
