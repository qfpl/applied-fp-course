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

If you're using Cabal 2.0 or greater. You can check your cabal version with `$ cabal --version`:

```shell
$ cabal new-configure --enable-tests
$ cabal new-build --enable-tests
$ cabal new-test
```

For a stack environment:

```shell
$ stack build --test
```

To load the tests in the REPL:

```shell
# Cabal
$ cabal new-repl app-fp-tests

# Stack
$ stack ghci applied-fp-course:test:app-fp-tests
```

To run the tests in the repl:

```shell
*Main> :main
```

Start in ``tests/Level03Tests.hs``.

[HSpec]: (http://hspec.github.io/)
[hspec-wai]: (https://hackage.haskell.org/package/hspec-wai)
[doctest]: (https://hackage.haskell.org/package/doctest)
