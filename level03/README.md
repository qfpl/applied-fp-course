# Level 03

In this exercise we build some configuration capabilities into our application.

This exercise will require a combination of building the right types to guide
your development, plus consulting plenty of documentation to leverage the chosen
packages. There may also be, depending on your level of interest, some external
reading for later as well.

Start in ``src/FirstApp/Conf.hs``.

The packages we will use for this are:

- [Aeson](http://hackage.haskell.org/package/aeson)
- [Optparse Applicative](http://hackage.haskell.org/package/optparse-applicative)

#### Aside: Tool Introduction - doctest

This level utilises the [doctest](https://hackage.haskell.org/package/doctest)
tool to help us ensure our functions comply with some quick tests that are
written as comments in the source file. This is a port of the same technology
that exists in Python.

You can see the new entry in the Cabal file as a ``test-suite``. The
``doctests.hs`` lists the files that have doctests that we want to run. The
``src/FirstApp/Conf.hs`` file contains some tests that you need to update as
part of the level.

For details on running and writing doctests, refer to the documentation.
