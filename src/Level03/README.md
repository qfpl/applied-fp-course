# Level 03

In this exercise we're going to add some tests to our application. Because types
are awesome, and tests are pretty good. But types AND tests is pretty much
perfect.

This exercise is to introduce you to testing your Haskell application. The setup
of the Cabal file is already completed for you, but will be covered.

As might be expected, there are multiple testing frameworks and packages
available. We will use the [tasty] framework, it is an established framework
that has widespread use. There are also several packages that extend [tasty]
with additional functionality, such as:

* `tasty-hunit` — for unit tests (based on HUnit)
* `tasty-golden` — for golden tests, which are unit tests whose results are kept in files
* `tasty-hedgehog` — for randomized property-based testing (based on Hedgehog)

We will be using the [tasty-wai] package to test our `Application`, as it takes
care of constructing the `Request`s. As well as providing a collection of
assertion functions we will use to verify our expectations.

For testing individual functions, there is the [tasty-hunit] package. It
provides functions for creating test cases and for checking your assertions

## NB: UNLIKE OTHER LEVELS

This level is not an isolated module to complete. This level exists as one
module: `tests/Test.hs`, into which you are to import your most recently
completed `Application`.

As you progress through the course, you are encouraged to return to this
`tests/Test.hs` and update it so you're able to be confident that your
application will behave as you expect. You may also write your tests before you
write your functions, this can be useful when trying to think through a problem.

For example, we will assume that you have just completed `Level04`. In order to
test it you will need to update the imports in `tests/Test.hs`:

```haskell
-- FROM:
import qualified Level02.Core as Core

-- TO:
import qualified Level04.Core as Core
```

#### Property-Based Testing [Optional]

For more advanced testing, there is the [hedgehog] property-based testing
package, and the [tasty] integration component [tasty-hedgehog]. 

Property based testing is a technique of testing whereby you specify the
properties that your function satisfies. Hedgehog then generates random input to
verify that your properties hold.

If there are inputs that cause your properties to _not_ hold, then Hedgehog will
attempt to shrink down the size of the inputs that broke your test, to try to
find the 'minimum' input required.

Property based testing is immensely effective at locating bugs and unexpected
behaviour and has lead to the notion of "property driven development".

#### Running the Tests

For a cabal sandbox:

```shell
$ cabal sandbox init
$ cabal install --only-dependencies --enable-tests
$ cabal configure --enable-tests
$ cabal test
```

If you're using Cabal 2.0 or greater (You can check your cabal version with `$ cabal --version`):

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

If you want to run the tests using the REPL, you can use the following command
in the REPL:

```shell
*Main> :main
```

Start in ``tests/Test.hs``.

[tasty]: (https://hackage.haskell.org/package/tasty)
[tasty-wai]: (https://hackage.haskell.org/package/tasty-wai)
[doctest]: (https://hackage.haskell.org/package/doctest)

