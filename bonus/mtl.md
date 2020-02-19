# MTL - Monad Transformer Library

This file is literate haskell. If you have `markdown-unlit` installed,
you can extract the code by running the following command:

```
$ markdown-unlit -h mtl.md mtl.md mtl.hs
```

This will allow you to work through the exercises using GHCi.


# Summary of the MTL Style

Overall, "MTL Style" means the following things for a piece of
functionality `Foo` (e.g., `Except`, `Reader`, `State`, &c):

1. A parcel of functionality bundled into a typeclasses `MonadFoo`;

2. A monad transformer `FooT`, which provides the `MonadFoo` instance
   which actually does the work;

3. Instances `MonadTrans FooT` and  `MonadIO m => MonadIO (FooT m)`;

4. A type alias `type Foo = FooT Identity`, for when an underlying
   monad is unnecessary; and

5. Instances of the form `MonadFoo m => MonadFoo (BarT m)` which lift
   `MonadFoo` functionality through other transformers.


# Building up to MTL

<details>
<summary>Extensions and Imports</summary>
```haskell
{-# OPTIONS_GHC -Wall -Wno-unused-imports #-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Applicative (liftA2)
import Data.Bifunctor (Bifunctor(..))
import Data.Either (either)
```
</details>

## Generalising `AppM`

Consider the `AppM` monad we defined in level 6:

```haskell
newtype AppM e a = AppM { runAppM :: IO (Either e a) }

instance Functor (AppM e) where
  fmap f = AppM . (fmap . fmap) f . runAppM

instance Applicative (AppM e) where
  pure = AppM . pure . pure

  AppM f <*> AppM a = AppM $ liftA2 (<*>) f a

instance Monad (AppM e) where
  AppM m >>= f = AppM $ m >>= either (pure . Left) (runAppM . f)
```

Let's look at this closely:

1. The `Functor` instance is completely uninteresting. We could have
   turned on `{-# LANGUAGE DeriveFunctor #-}` and written `deriving
   Functor`, and GHC would generate the same code.

2. The `Applicative` instance is slightly more interesting, but not
   very. It is always fundamentally the same: composing two
   `Applicative`s always yields an `Applicative`. Here, that's the
   `IO` and `Either e` `Applicative`s, but this fact is witnessed in
   general by the `instance (Applicative f, Applicative g) =>
   Applicative (Compose f g)` in
   [`Data.Functor.Compose`](https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor-Compose.html#t:Compose).

3. The `Monad` instance is pattern-matching some `Either`-flavoued
   stuff, and calling `(>>=)`. It is using no `IO`-specific features.

This means that we can generalise the `AppM` instance over any
underlying monad:

```haskell
-- Exercise: What are the constraints on the Functor instance? Check with `:i`.
newtype AppM' m e a = AppM' { runAppM' :: m (Either e a) } deriving Functor

instance Applicative m => Applicative (AppM' m e) where
  pure = AppM' . pure . pure
  AppM' f <*> AppM' a = AppM'$ liftA2 (<*>) f a

-- Exercise: Implement this
instance Monad m => Monad (AppM' m e) where
  (>>=) :: AppM' m e a -> (a -> AppM' m e b) -> AppM' m e b
  (>>=) = error "(>>=) -- AppM'"
```

<details>
<summary>Solution</summary>
```haskell ignore
instance Monad m => Monad (AppM' m e) where
  AppM' m >>= f = AppM' $ m >>= either (pure . Left) (runAppM' . f)
```
</details>


## `AppM` is the Wrong Name

At this point, we've found something very general, and it's no longer
appropriate to call it `AppM`. Changing the order of the type
variables, shows that we have rediscovered `ExceptT` from
`transformers`'s `Control.Monad.Trans.Except` module:

```haskell
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
  deriving Functor
```

<details>
<summary>Instances</summary>
```haskell
instance Applicative m => Applicative (ExceptT e m) where
  pure = ExceptT . pure . pure
  ExceptT f <*> ExceptT a = ExceptT $ liftA2 (<*>) f a

instance Monad m => Monad (ExceptT e m) where
  ExceptT m >>= f = ExceptT $ m >>= either (pure . Left) (runExceptT . f)
```
</details>

`ExceptT e` is a _monad transformer_: if `m` is a monad, then so is
`ExceptT e m`.


## What if I don't want to Transform Anything?

`mtl` provides aliases that use the `Identity` functor as the base monad:

```haskell
-- From Data.Functor.Identity
newtype Identity a = Identity { runIdentity :: a } deriving Functor

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  Identity a >>= f = f a

-- Type aliases are eta-reduced as far as possible, for maximum usefulness.
-- (GHC can't expand a type alias until it's fully applied.)
type Except e = ExceptT e Identity
type Reader r = ReaderT r Identity
```

## Other Transformers

 `transformers` provides a family of these transformers, and by
stacking them atop each other, you can build up a monad for your
needs. We will only talk about two: `ExceptT`, which we just
discovered; and `ReaderT`, which passes around an argument for
us. `ReaderT r` is a monad transformer: if `m` is a monad, `ReaderT r
m` is also a monad:

```haskell
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a } deriving Functor

-- Excercise: Write the Applicative and Monad instances
instance Applicative m => Applicative (ReaderT r m) where
  pure :: a -> ReaderT r m a
  pure = error "pure -- ReaderT r m"

  (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b
  (<*>) = error "(<*>) -- ReaderT r m"

instance Monad m => Monad (ReaderT r m) where
  (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (>>=) = error "(>>=) -- ReaderT r m"
```

<details>
<summary>Solution</summary>
```haskell ignore
instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  ReaderT f <*> ReaderT a = ReaderT $ liftA2 (<*>) f a

instance Monad m => Monad (ReaderT r m) where
  ReaderT m >>= f = ReaderT $ \r -> do
    a <- m r
    runReaderT (f a) r
```
</details>


## Adding Features to Transformers

Now that we have these general type definitions, we want to be able to
provide useful functions alongside them.

```haskell
-- Return the environment.
ask :: Monad m => ReaderT r m r
ask = error "ask"

-- Apply a function to the environment, and return it.
asks :: Monad m => (r -> a) -> ReaderT r m a
asks = error "asks"

-- Run a subcomputation in a modified environment.
-- This is a specialisation of withReaderT.
local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local = error "local"

-- Transform the environment of a reader.
withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT = error "withReaderT"

throwError :: Applicative m => e -> ExceptT e m a
throwError = error "throwError"

catchError :: Monad m => ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
catchError = error "catchError"

-- Transform the error type.
withExceptT :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT = error "withExceptT"

-- Transform the unwrapped computation.
mapExceptT
  :: (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
mapExceptT = error "mapExceptT"

-- Lift a "catchError"-shaped function through a ReaderT.
-- We will need this later when we introduce MTL typeclasses.
liftCatch
  :: (m a -> (e -> m a) -> m a)
  -> ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a
liftCatch = error "liftCatch"
```

<details>
<summary>Solution</summary>
```haskell ignore
ask :: Monad m => ReaderT r m r
ask = ReaderT pure

asks :: Monad m => (r -> a) -> ReaderT r m a
asks f = f <$> ask

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local = withReaderT

withReaderT :: (r' -> r) -> ReaderT r m a -> ReaderT r' m a
withReaderT f (ReaderT m) = ReaderT $ m . f

mapExceptT
  :: (m (Either e a) -> n (Either e' b)) -> ExceptT e m a -> ExceptT e' n b
mapExceptT f = ExceptT . f . runExceptT

throwError :: Applicative m => e -> ExceptT e m a
throwError = ExceptT . pure . Left

-- Notice how similar this is to the definition of (>>=) for ExceptT.
-- If this interests you, check out this paper:
-- "Exceptionally Monadic Error Handling" - https://arxiv.org/pdf/1810.13430.pdf
catchError :: Monad m => ExceptT e m a -> (e -> ExceptT e m a) -> ExceptT e m a
catchError (ExceptT m) f = ExceptT $ m >>=
  either (runExceptT . f) (pure . Right)

withExceptT :: Functor m => (e -> e') -> ExceptT e m a -> ExceptT e' m a
withExceptT f = ExceptT . fmap (first f) . runExceptT

liftCatch
  :: (m a -> (e -> m a) -> m a)
  -> ReaderT r m a -> (e -> ReaderT r m a) -> ReaderT r m a
liftCatch catch (ReaderT m) f = ReaderT $ \r ->
  catch (m r) (\e -> runReaderT (f e) r)
```
</details>

## The `MonadTrans` Typeclass

Now that we have this vocabulary of operations tied to each
transformer, it is often useful to be able to lift them through other
transformers, by ignoring the features added by the extra tranformers:

```haskell
-- MonadTrans has the following laws, which show that `t` does indeed transform
-- `m`, and does so in a way that doesn't use the features of `t`.
--
-- 1. lift . pure = pure
-- 2. lift (m >>= f) = lift m >>= lift . f
class MonadTrans t where
  lift :: Monad m => m a -> t m a

-- Exercise: Write MonadTrans instances for `ReaderT r` and `ExceptT e`.
instance MonadTrans (ReaderT r) where
  lift :: m a -> ReaderT r m a
  lift = error "lift -- ReaderT r m"

instance MonadTrans (ExceptT e) where
  lift :: m a -> ExceptT e m a
  lift = error "lift -- ExceptT e m"
```

<details>
<summary>Solution</summary>
```haskell ignore
instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance MonadTrans (ExceptT e) where
  lift = ExceptT . fmap Right
```
</details>

The `MonadIO` typeclass is a special case of this, which uses the fact
that there's no such thing as `IOT` to lift an `IO a` all the way up
the stack:

```haskell
class Monad m => MonadIO m where
  liftIO :: IO a -> m a

-- Exercise: Write MonadIO instances for `ReaderT r m` and `ExceptT e m`,
-- assuming `MonadIO m`. Use `lift` instead of plumbing through the
-- transformers explicitly.
instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO :: IO a -> ReaderT r m a
  liftIO = error "liftIO -- ReaderT r m"

instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO :: IO a -> ExceptT r m a
  liftIO = error "liftIO -- ExceptT r m"
```

<details>
<summary>Solution</summary>
```haskell ignore
instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO = lift . liftIO
```
</details>


## Writing `lift` Everywhere Stinks!

When you stack up a few transformers, it can get annoying writing
`lift` everywhere to lift operations from the lower transformers up
through the stack. Is there a better way?

The answer is yes. The trick is to define typeclasses that do the
lifting for you, automatically. These are _multi-parameter
typeclasses_ (MPTCs). If you think of a normal typeclass as a
predicate over types, then an MPTC relates two or more types with each
other. An instance `MonadError e m` means that we can throw and catch
errors of type `e` in our monad `m`.

The other syntax that may be new here is the `| m -> r`. This is
called a _functional dependency_ or "fundep" and tells GHC two things:

1. There will never be two instances for `MonadReader r m` that have
   the same `m` but different `r`.

2. Because of this, if the typechecker determines `m`, it can
   immediately conclude what `r` must be.

The upside of this is that GHC's typechecker can automatically lift
operations through other transformers for you. The downside is that
you cannot have two `ExceptT` layers (say) in the same stack. If your
immediate response is to say "but I want to do that!", check out the
"classy MTL" pattern.

```haskell
-- The functions in MTL do not have the trailing prime symbol (`'`),
-- but I must avoid duplicate definitions.
class Monad m => MonadReader r m | m -> r where
  ask' :: m r
  asks' :: (r -> a) -> m a
  local' :: (r -> r) -> m a -> m a

class Monad m => MonadError e m | m -> e where
  throwError' :: e -> m a
  catchError' :: m a -> (e -> m a) -> m a

-- Exercise: write these four instances.
--
-- The first two instances implement the class operations for each transformer.
-- You have written these functions already.
--
-- The second two instances lift the functionality of one
-- transformer through another.
instance Monad m => MonadReader r (ReaderT r m) where
  ask' = error "ask' -- ReaderT r m"
  asks' = error "asks' -- ReaderT r m"
  local' = error "local' -- ReaderT r m"

instance Monad m => MonadError e (ExceptT e m) where
  throwError' = error "throwError' -- ExceptT e m"
  catchError' = error "catchError' -- ExceptT e m"

instance MonadReader r m => MonadReader r (ExceptT e m) where
  ask' = error "ask' -- lift through ExceptT"
  asks' = error "asks' -- lift through ExceptT"
  local' = error "local' -- lift through ExceptT"

instance MonadError e m => MonadError e (ReaderT r m) where
  throwError' = error "throwError' -- lift through ReaderT"
  catchError' = error "catchError' -- lift through ReaderT"
```

<details>
<summary>Solution</summary>
```haskell ignore
instance Monad m => MonadReader r (ReaderT r m) where
  ask' = ask
  asks' = asks
  local' = local

instance Monad m => MonadError e (ExceptT e m) where
  throwError' = throwError
  catchError' = catchError

instance MonadReader r m => MonadReader r (ExceptT e m) where
  ask' = lift ask'
  asks' = lift . asks'
  local' = mapExceptT . local'

instance MonadError e m => MonadError e (ReaderT r m) where
  throwError' = lift . throwError'
  catchError' = liftCatch catchError'
```
</details>


## Writing all the Instances Stinks!

Yes. This is called the "O(n^2) instances" problem. Each bundle of
monad operations wants its own class, instances of that class for one
or more transformers (if you have multiple transformers providing
different implementations), plus instances that lift through every
other possible transformer.

This is a lot of boilerplate, and it's important to know two things:

1. It's not always possible to lift one instance through
   another. Such instances do not and should not exist.

2. Monad transformers, in general, do not commute. `StateT s (ExceptT
   e m)` is a different beast to `ExceptT e (StateT s m)` - the former
   throws away the state on error, while the latter preserves it.

Algebraic effect systems attempt to cut down boilerplate, but make
compromises of their own. There is active research in this area.


## `AppM` Revisited

The payoff of all this setup (helpfully encapsulated in the `mtl`
library, which builds atop `transformers`) is that we can specify our
application monad as a stack of transformers atop `IO`, and use GHC's
`GeneralizedNewtypeDeriving` extension to give us all the instances
immediately:

```haskell
data Env -- Dummy type for the sake of example

-- This is the Level07 AppM, which also passes around an environment
newtype AppMtl e a = AppMtl (ReaderT Env (ExceptT e IO) a)
  deriving (Functor, Applicative, Monad, MonadError e, MonadReader Env)

runAppMtl :: AppMtl e a -> Env -> IO (Either e a)
runAppMtl (AppMtl m) env = runExceptT $ runReaderT m env
```


## A note on Perfomance

Monad transformers are a great way to rapidly set up application
monads, and a well-placed `runFooT` call can let you temporarily pick
up extra functionality where it makes sense. Sometimes, GHC is not
smart enough to inline all the dictionary passing, and this may slow
down your program.

Should this happen to you (and be proven by profiling), consider
defining your application monad directly and only then implementing
the `MonadFoo` instances by hand.
