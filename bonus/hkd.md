# HKD - Higher-Kinded Data

This file is literate haskell. If you have
[`markdown-unlit`](https://hackage.haskell.org/package/markdown-unlit)
installed, you can extract the code by running the following command:

```
$ markdown-unlit -h mtl.md mtl.md mtl.hs
```

This will allow you to work through the exercises using GHCi.

In this document, we will explore a pattern called "higher-kinded
data", which is when a data structure is parameterised by a
`Functor`. This is becoming increasingly common in libraries like
[`dependent-sum`](https://hackage.haskell.org/package/dependent-sum)
and
[`dependent-map`](https://hackage.haskell.org/package/dependent-map),
and so is worth knowing about.


<details>
<summary>Extensions and Imports</summary>

```haskell
{-# OPTIONS_GHC -Wall -Wno-unused-imports -Wno-unused-matches #-}

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Functor.Compose (Compose(..))
import Data.Functor.Identity (Identity(..))
import qualified Data.Semigroup as Semigroup
import Data.Kind (Type)
import Data.Word (Word16)
```

</details>


## Generalising `Conf`

Consider the configuration types from level 7:

```haskell
newtype Port = Port Word16

data Conf = Conf
  { port       :: Port
  , dbFilePath :: FilePath
  }

data PartialConf = PartialConf
  { pcPort       :: Maybe (Semigroup.Last Port)
  , pcDBFilePath :: Maybe (Semigroup.Last FilePath)
  }
```

There's repeated structure here. We can generalise over the both of
these structures by introducing a `Functor`-kinded type parameter:

```haskell
data Config f = Config
  { _port :: f Port
  , _dbFilePath :: f FilePath
  }

-- It is sometimes useful to provide a convenience function that uses
-- pure to add all the `f` wrappers.
-- Exercise: implement it.
config :: Applicative f => Port -> FilePath -> Config f
config = error "config"

-- The forall in the constraint uses -XQuantifiedConstraints, available since
-- GHC 8.6.1 You could instead explicitly list them out instead:
-- instance (Show (f Port), Show (f FilePath)) => ...
deriving instance (forall a . Eq (f a)) => Eq (Config f)
deriving instance (forall a . Show (f a)) => Show (Config f)

-- Exercise: implement these instances
instance (forall a . Semigroup (f a)) => Semigroup (Config f) where
  (<>) = error "(<>) -- Config f"

instance (forall a . Monoid (f a)) => Monoid (Config f) where
  mempty = error "mempty -- Config f"
```

<details>
<summary>Solution</summary>

```haskell ignore
config :: Applicative f => Port -> FilePath -> Config f
config p db = Config (pure p) (pure db)

instance forall a . Semigroup (f a) => Semigroup (Config f) where
  Config p1 db1 <> Config p2 db2 = Config (p1 <> p2) (db1 <> db2)

instance forall a . Monoid (f a) => Monoid (Config f) where
  mempty = Config mempty mempty
```

</details>

<details>
<summary>Aside: `forall a . Semigroup (f a)` vs `Alternative f`</summary>

You might well be wondering, "doesn't
[`Alternative`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Applicative.html#t:Alternative)
describe a monoid on applicative functors?" The answer is yes. I have
chosen to use `Semigroup`/`Monoid` constraints in the superclass for
a few reasons:

1. `First` and `Last` have no `Alternative` instance.
2. `Alternative` is not just a monoid on applicative functors, it is
   also a statement of intent: it's a class with a "control structure"
   sort of flavour. We are talking about data here, so I feel that
   asking for `Semigroup`/`Monoid` instances more appropriate.

</details>


## Specific `Conf`s

Now that we have our `Config` structure, we can recover both full and
partial configs by choosing an appropriate functor, and writing a
function to (maybe) extract a full config from a partial one:

```haskell
-- Conf' and PartialConf' are primed to not clash with the level 7 ones.
type Conf' = Config Identity -- 'Identity' is defined in 'Data.Functor.Identity'

-- This is Data.Maybe.Last, which will eventually be deprecated and removed.
-- Using `Maybe (Last a)` is the forward-compatible recommendation but we can't
-- do that here just yet; see https://gitlab.haskell.org/ghc/ghc/issues/17859
-- for the gory details.
newtype Last a = Last { getLast :: Maybe a }

instance Semigroup (Last a) where
  Last (Just a) <> Last Nothing = Last $ Just a
  _ <> b = b

instance Monoid (Last a) where
  mempty = Last Nothing

type PartialConf' = Config Last

-- Exercise: implement this
extractConfig :: PartialConf' -> Maybe Conf'
extractConfig = error "extractConfig"
```

<details>
<summary>Solution</summary>

```haskell ignore
extractConfig :: PartialConf' -> Maybe Conf'
extractConfig (Config (Last mp) (Last mdb)) = Config <$> mp <*> mdb
```

</details>


## Wait a Minute...

Let's look at the expanded type of `extractConfig`:

```haskell ignore
extractConfig :: PartialConf' -> Maybe (Conf')
extractConfig :: Config Last -> Maybe (Config Identity) -- expand type synonyms
extractConfig :: Config Maybe -> Maybe (Config Identity) -- 'Last' is representationally 'Maybe'
```

Does this remind you of something? It looks like `sequence`, but over
a structure that takes a functor (kind `Type -> Type`) instead of a
type of kind `Type` as its final parameter:

```haskell ignore
sequence
  :: (Applicative f, Traversable t)
  =>                  t (f a) ->     f (t a)
extractConfig :: Config Maybe -> Maybe (Config Identity)
```

(Ignore the extra `Identity` noise for now.)

Can we use this observation? You betcha!


## Rank 2 `Functor`

Having seen this pattern, let's see if we can build a typeclass
hierarchy towards it, starting with good ol' `Functor`. We want a
typeclass for which types of `Config`'s kind can be instances, so it's
going to look something like:

```haskell
-- This is (roughly) Rank2.Functor from the `rank2classes` package.
class Rank2Functor (g :: (Type -> Type) -> Type) where
  r2fmap :: (forall a . p a -> q a) -> g p -> g q

instance Rank2Functor Config where
  r2fmap :: (forall a . p a -> q a) -> Config p -> Config q
  r2fmap f = error "r2fmap -- Config"
```

The `Functor` we're used to lifts a function `(a -> b)` into a
function `(f a -> f b)`. But we want to map over the parameter to
`Config`, so if we can turn a functor `p` into a functor `q`, we can
turn a `Config p` into a `Config q`. This is the meaning of the
`(forall a . p a -> q a)` argument; we want a function that turns `p a`
into `q a` without knowing what `a` is. Because it has a `forall`
inside parentheses, we call this a "rank-2 function".

In a normal function like `map :: (a -> b) -> [a] -> [b]`, we have no
control over the types of `a` and `b`, and must work with what we're
given. In the rank-2 parameter `(forall a . p a -> p a)`, we have no
control over `p` or `q`, but the _caller_ of `r2fmap` must provide a
function that has no control over `a`. This means we can use the
single function we're given to map `p Port` into a `q Port` and a `p
FilePath` into a `q FilePath`.

**Aside:** Functions of type `(forall a . p a -> q a)` are sometimes
called _natural transformations_, after the concept in category
theory. A natural transformation in category theory turns one functor
into another, but is a weaker concept than the type `(forall a . p a
-> q a)`. (Natural transformations in category theory are allowed to
do different things at each object, whereas a function `(forall a . p
a -> q a)` must do the same thing regardless of the type of `a`.)

<details>
<summary>Solution</summary>

```haskell ignore
instance Rank2Functor Config where
  r2fmap f (Config p db) = Config (f p) (f db)
```

</details>


## Rank 2 `Traversable`

We're going to skip over `Rank2Foldable` (it exists in `rank2classes`)
and go straight to `Rank2Traversable`. This means our version will
have slightly different superclasses:

```haskell
-- This is (roughly) `Rank2.Traversable` from `rank2classes`.
class Rank2Functor g => Rank2Traversable g where
  {-# MINIMAL sequence | traverse #-}

  -- The type of 'sequence' contains a 'Compose' because we need to make
  -- sure there's a functor underneath the `m` we use for our effects.
  -- (Even if that underlying functor is something boring like `Identity`.)
  -- Exercise: implement 'sequence' using 'traverse'.
  r2sequence :: Applicative m => g (Compose m p) -> m (g p)
  r2sequence = error "r2sequence -- Rank2Traversable"

  -- Exercise: implement 'traverse' using 'sequence'.
  r2traverse :: Applicative m => (forall a . p a -> m (q a)) -> g p -> m (g q)
  r2traverse f = error "r2traverse -- Rank2Traversable"

-- Exercise: implement both methods of Rank2Traversable.
instance Rank2Traversable Config where
  r2sequence :: Applicative m => Config (Compose m p) -> m (Config p)
  r2sequence = error "r2sequence -- Config"

  r2traverse
    :: Applicative m
    => (forall a . p a -> m (q a))
    -> Config p
    -> m (Config q)
  r2traverse f = error "r2traverse -- Config"
```

<details>
<summary>Solution</summary>

```haskell ignore
class Rank2Functor g => Rank2Traversable g where
  {-# MINIMAL sequence | traverse #-}

  r2sequence :: Applicative m => g (Compose m p) -> m (g p)
  r2sequence = r2traverse getCompose

  r2traverse :: Applicative m => (forall a . p a -> m (q a)) -> g p -> m (g q)
  r2traverse f = r2sequence . r2fmap (Compose . f)

-- Exercise: implement both methods of Rank2Traversable.
instance Rank2Traversable Config where
  r2sequence :: Applicative m => Config (Compose m p) -> m (Config p)
  r2sequence (Config p db) = Config <$> getCompose p <*> getCompose db

  r2traverse
    :: Applicative m
    => (forall a . p a -> m (q a))
    -> Config p
    -> m (Config q)
  r2traverse f (Config p db) = Config <$> f p <*> f db
```


## Payoff

We can now generalise our `extractConfig` to any `Rank2Traversable`:

```haskell
-- Exercise: implement this
fromLast :: Rank2Traversable g => g Last -> Maybe (g Identity)
fromLast = error "fromLast"
```

<details>
<summary>Solution</summary>

```haskell ignore
fromLast :: Rank2Traversable g => g Last -> Maybe (g Identity)
fromLast = r2traverse $ r2fmap Identity . getLast
```


## Other Functors

Now that we have this machinery, we can ask what happens if we use
other functors? A `Config IO`, for example, contains actions to fetch
each component, and by `r2traverse`-ing over the `Config`, we can
construct an `IO` action that builds up an entire `Config Identity`.

**Exercise:** Pick some other functors, apply them to `Config`, and
imagine what they might mean.


# Is this Worthwhile?

Was this more than just a fun mental exercise? I think
so. `rank2classes` provides most of the typeclass machinery we
implemented in this document, and Template Haskell functions to
generate instances for your data types.

Whether I use this technique in a real project depends on two things:

1. Most importantly, whether the rest of the team is comfortable with
   the idea; and

2. Whether or not I intend to exploit the functor parameter.

For the `Config` data type we explored in this document, I probably
would use higher-kinded data if the team was okay with it.


# Further Reading

* Benjamin Hodgson - [Functor Functors](https://www.benjamin.pizza/posts/2017-12-15-functor-functors.html)
* [`rank2classes` on Hackage](https://hackage.haskell.org/package/rank2classes)
* [`conkin` on Hackage](https://hackage.haskell.org/package/conkin)
* [`dependent-sum` on Hackage](https://hackage.haskell.org/package/dependent-sum)
* [`dependent-map` on Hackage](https://hackage.haskell.org/package/dependent-map)
