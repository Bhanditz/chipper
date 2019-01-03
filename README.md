# chipper

Enchips things.

## Is this any good?

No. There is not much here yet.

## Install notes

Haskell stack is needed to build.

`stack build` in the base of the repo should build it; `stack install` to install it.

Note that at present stack makes cabal files that are incompatible with resolver: lts-11.2, which is needed to get some random dep building. Edit the version field in the generated `chipper.cabal` file manually to 'cabal-version: 2.0.1.0'.
