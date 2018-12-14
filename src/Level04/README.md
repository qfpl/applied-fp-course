# Level 04

#### Database Integration

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

#### Testing with a database

When testing your application it is wise to use a database that is created
explicitly for testing. When using `sqlite-simple`, we can either specify a
different file path to the database file we want to use. Alternatively we can
use a purely "in-memory" database, that won't persist and will always be able to
be created for our tests.

To do this, provide the special file path `":memory:"`. The `sqlite-simple`
package will pass this to SQLite and it will create a table in memory. You may
also provide an empty string, although that is a bit ambiguous.

More information can be found at:

- [Hackage docs for open](https://hackage.haskell.org/package/sqlite-simple-0.4.16.0/docs/Database-SQLite-Simple.html#v:open)
- [SQLite Documentation](https://www.sqlite.org/inmemorydb.html)

This should suffice for our needs in the course, but when it comes to larger
applications that have tests that hit a database. You will want to more
explicitly manage a separate database that does persist. In case you want to
examine the data for debugging.

#### JSON Encoding

Now that we have a place to keep our `Topic`s and `Comment`s, we need to be able
to encode this data in a way that is acceptable for other systems to consume.
JSON is de rigueur so we won't buck the trend just yet.

We will be using the [waargonaut](https://hackage.haskell.org/package/waargonaut)
package to do the heavy lifting for us. You are required to write the encoder
functions necessary to describe our types to Waargonaut.

We will be building an `Encoder` for our `Topic`, `CommentText`, and `Comment`
types. The `Encoder` and `Decoder` functions can be combined to handle more
complicated structures built of smaller components. An `Encoder` for `Text` can
be combined with the `Encoder` for `[]` to create an `Encoder` for `[Text]`, for
example.

## NB: We will not necessarily provide all of the required imports!

There may be other things you have to bring into scope. So if you see a compiler
error of the sort: "X is not in scope." then you may need to import a type or
function into the module scope.

## Steps for this level:

The steps for this level:
1) ``src/Level04/Types/Topic.hs``
2) ``src/Level04/Types/CommentText.hs``
3) ``src/Level04/DB/Types.hs``
4) ``src/Level04/Types.hs``
5) ``src/Level04/DB.hs``
6) ``src/Level04/Core.hs``

For the sake of simplicity, any configuration requirements will be hardcoded in
``Level04/Conf.hs`` for now. We will return to that in a future level.

# Useful Typeclasses

## [Contravariant](http://hackage.haskell.org/package/contravariant/docs/Data-Functor-Contravariant.html)

The `Contravariant` typeclass provides the following function:

```haskell
contramap :: Contravariant f => (a -> b) -> f b -> f a
```

This might seem super wild, but if you take a moment, follow the types, and
perhaps squint a bit. We're able to discern that:

1) If we provide:
  * some way of going from an `a` to a `b`: `(a -> b)` 
  * and a `f b`

2) We're able to create `f a` by applying the `(a -> b)` to the `a` so that we
   then have a `b`

We will work through a small example. Copied from the `Contravariant` documentation on Hackage:
https://hackage.haskell.org/package/contravariant-1.5/docs/Data-Functor-Contravariant.html#t:Contravariant.

As an example, consider the type of predicate functions a -> Bool. One such
predicate might be negative x = x < 0, which classifies integers as to whether
they are negative. However, given this predicate, we can re-use it in other
situations, providing we have a way to map values to integers. For instance, we
can use the negative predicate on a person's bank balance to work out if they
are currently overdrawn:

```haskell
newtype Predicate a = Predicate { getPredicate :: a -> Bool }

instance Contravariant Predicate where
  contramap f (Predicate p) = Predicate (p . f)
                                         |   `- First, map the input...
                                         `----- then apply the predicate.

overdrawn :: Predicate Person
overdrawn = contramap personBankBalance negative
```

## [Traversable](https://hackage.haskell.org/package/base/docs/Data-Traversable.html)

This typeclass provides a function called ``traverse``, which is for
traversing a structure from left to right, performing an action on each
element. This is its type:

```haskell
traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
```

This is very useful when you want to perform an action on every element of a
list, but that action will return the new result in an ``Applicative``
context.

To help build some intuition for how this function can be useful, write out
the type signature for ``traverse``, but replace the ``f`` with ``Either e``,
and the ``t`` with ``[]``. This type of exercise is useful to help build your
understanding of how the pieces fit together for generic functions such as
this one.

Can you explain why you replace ``f`` with ``Either e`` and not ``Either``?

## [Bifunctor](https://hackage.haskell.org/package/base/docs/Data-Bifunctor.html)

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
