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
context. Such as ``IO`` or ``Either``. By way of example, if we have the
following:

```haskell
possibleTopics :: [Text]
mkTopic :: Text -> Either Error Topic
```

We can use ``traverse`` to apply our function to every item in the list, but
abort should any of them fail, courtesy of the ``Applicative`` instance for
``Either``.

```haskell
# If we partially apply our traverse with mkTopic:
( traverse mkTopic ) :: Traversable t => t Text -> Either Error (t Topic)

# Then add the list of possibleTopics:
( traverse mkTopic possibleTopics ) :: Either Error [Topic]
```

Contrast this with using ``fmap``:

```haskell
# Recall that fmap has the type:
fmap :: Functor f => (a -> b) -> f a -> f b

# Partially applied with mkTopic
( fmap mkTopic ) :: Functor f => f Text -> f (Either Error Topic)

# Over the list of possibleTopics
( fmap mkTopic possibleTopics ) :: [Either Error Topic]
```

Both techniques are valid, but by using ``traverse`` we are able to more
easily compose the result with something that may need a ``[Topic]``. As
opposed to using ``fmap`` that requires us to still handle the errors
individually. In this instance, the trade-off is that using ``traverse`` will
fail on the first error, whereas ``fmap`` will provide a list that still may
have errors in it.

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
