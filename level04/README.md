# Level 04

In this exercise we're going to add some tests to our application. Because types
are awesome, and tests are pretty good. But types AND tests is pretty much
perfect.

These tests will not be awe inspiring, this exercise is primarily to introduce
you to adding tests to your Haskell application. The setup of the Cabal file is
already completed for you, but will be covered.

As is to be expected, there are multiple testing frameworks and packages
available but we will only cover one here. We will use the [HSpec] framework,
with the [hspec-wai] package to make our lives a bit easier.

Start in ``tests/Test.hs``.

#### Aside: Tool Introduction - ghcid

Additionally we'd like to introduce a command line tool that you may find useful
for Haskell development; [ghcid]. This is a very lightweight tool that works for
any project with a functioning cabal setup.

If you would like to use it, consult its documentation for how to install it,
and then in an spare open terminal window, navigate to the root of the Haskell
project and run ``$ ghcid``.

It will then attempt to build your project, if errors are found they will be
displayed. But more importantly you can go back to editing files in the project
and ``ghcid`` will refresh in the background. Providing you with new error
messages or ``All Good`` if it cannot find any errors.

``ghcid`` provides extremely fast feedback, allowing for a nice development
process with constant feedback about your changes. It is very useful in tandem
with type holes. Give it a try!

[HSpec]: (http://hspec.github.io/)
[hspec-wai]: (https://hackage.haskell.org/package/hspec-wai)
[ghcid]: (https://github.com/ndmitchell/ghcid)
