# Level 04

We need a place to store our Comments/Topics, so we're going to add a database
to our application, specifically the SQLite database. We've chosen SQLite
because it was the simplest to have up and running for the purposes of the
course.

There is an example module included for using PostgreSQL, however the course
will focus on the SQLite implementation.

For reference, the packages we will use to talk to our database are:

- [sqlite-simple](https://hackage.haskell.org/package/sqlite-simple)
- [sqlite-simple-errors](https://hackage.haskell.org/package/sqlite-simple-errors)

You will also need the [SQLite](https://www.sqlite.org/) database application
installed and available on your system.

Also we will not necessarily provide all of the required imports any more, there
may be other things you have to bring into scope.

The steps for this level:
1) ``src/FirstApp/DB/Types.hs``
2) ``src/FirstApp/Types.hs``
3) ``src/FirstApp/DB.hs``
4) ``src/FirstApp/Main.hs``

For the sake of simplicity, any configuration requirements will be hardcoded in
``FirstApp/Conf.hs`` for now. We will return to that in the next level.

NB: The PostgreSQL example module is in ``src/FirstApp/DB/PostgreSQL.hs``.

# Useful Typeclasses

## [Traversable](hackage.haskell.org/package/base/docs/Data-Traversable.html)

This typeclass provides a function called ``traverse``, which is for
traversing a structure from left to right, performing an action on each
element. This is its type:

```haskell
traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
```

This is very useful when you want to perform an action on every element of a
list, but that action will return the new result in a some ``Applicative``
context. 

To help build some intuition for how this function can be useful, write out
the type signature for ``traverse``, but replace the ``f`` with ``Either e``,
and the ``t`` with ``[]``. This type of exercise is useful to help build your
understanding of how the pieces fit together for generic functions such as
this one.

Can you explain why you replace ``f`` with ``Either e`` and not ``Either``?

## [Bifunctor](hackage.haskell.org/package/base/docs/Data-Bifunctor.html)

As you may be able to work out from the name, this typeclass describes
structures that have two things we can treat like a ``Functor``. The ones
that we're probably going to be interested in are:

```haskell
# ``fmap`` over both sides of the Bifunctor
bimap :: Bifunctor p => (a -> b) -> (c -> d) -> p a c -> p b d

# ``fmap`` over one particular side, respectively:
first :: Bifunctor p => (a -> b) -> p a c -> p b c
second :: Bifunctor p => (c -> d) -> p a c -> p a d
```

For a concrete example, we might want to run functions over both elements in
a tuple. We cannot use ``fmap`` for this as the types only allow us to affect
one type variable:

```haskell
bimap (<> "b") (+1) ("a", 3) = ("ab", 4)
```

Similarly, if we only want to run a function on the ``Left`` value of an Either:

```haskell
# Given
wrapInnerError :: InnerErr -> AppErr

# Partially applied with ``first``
first wrapInnerError :: Either InnerErr b -> Either AppErr b
```
