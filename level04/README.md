# Level 04

We're going to add some tests to our application! Types are awesome, tests are
pretty good. Types AND tests is pretty much perfect.

These tests will not be awe inspiring, this exercise is primarily to introduce
you to adding tests to your Haskell application. The setup of the Cabal file is
already completed for you, but will be covered.

As is to be expected, there are multiple testing frameworks and packages
available but we will only cover one here. Chosen partly due to its possible
familiarity.

We will use the [HSpec](http://hspec.github.io/) framework, with
the [hspec-wai](https://hackage.haskell.org/package/hspec-wai) package to make
our lives a bit easier.

Start in ``tests/Test.hs``.
