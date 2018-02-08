# Level 03

In this exercise we're going to add some tests to our application. Because types
are awesome, and tests are pretty good. But types AND tests is pretty much
perfect.

These tests will not be awe inspiring, this exercise is primarily to introduce
you to adding tests to your Haskell application. The setup of the Cabal file is
already completed for you, but will be covered.

As is to be expected, there are multiple testing frameworks and packages
available but we will only cover one here. We will use the [HSpec] framework,
with the [hspec-wai] package to make our lives a bit easier.

### Including Test Library Dependencies and Running the Tests

For a cabal sandbox:

```shell
$ cabal sandbox init
$ cabal install --only-dependencies --enable-tests
$ cabal configure --enable-tests
$ cabal test
```

For a stack environment:

```shell
$ stack build --test
```

To load the tests in the REPL:

```shell
# Cabal
$ cabal repl level03-tests

# Stack
$ stack ghci level03:level03-tests
```

To run the tests in the repl:

```shell
*Main> :main
```

Start in ``tests/Test.hs``.

[HSpec]: (http://hspec.github.io/)
[hspec-wai]: (https://hackage.haskell.org/package/hspec-wai)
[doctest]: (https://hackage.haskell.org/package/doctest)
