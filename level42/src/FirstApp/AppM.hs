{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module FirstApp.AppM where

import           Control.Applicative    (liftA2)
import           Control.Monad          ((>=>))
import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..))

import           Data.Text              (Text)

import           FirstApp.DB.Types      (FirstAppDB)
import           FirstApp.Types         (Conf, Error)

import           Data.Bifunctor         (first)

data Env = Env
  { envLoggingFn :: Text -> AppM ()
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

-- We're going to add a very useful abstraction to our application. We'll
-- automate away the explicit error handling and inspection of our Either values
-- while preserving the type-level information that tells us what can go wrong.
--
-- To do this we will expand the capabilities of our AppM by including the
-- Either type in our definition. We will also rework our Monad instance to stop
-- processing when it encounters a Left value.
--
-- This will work in the same manner as the Functor/Applicative/Monad
-- instances for Either, with functions being applied to the Right value and
-- everything been ignored if a Left value is encountered, returning that Left
-- value.
--
-- f <$> (Left e)  = Left e
-- f <$> (Right a) = Right (f a)
--
-- (Left e)  >>= f = Left e
-- (Right a) >>= f = f a
--
-- This means when we have a function doing this sort of shuffling:
--
-- foo :: IO (Either Error Value)
-- foo = do
--   aE <- mightFail
--   either (pure . Left) needsAButMightFail aE
--   where
--     mightFail :: IO (Either Error Int)
--     alsoMightFail :: Int -> IO (Either Error Value)
--
-- We can wrap our functions with AppM and we can work directly with the
-- values we expect to appear on the happy path, knowing that if the sad path is
-- encountered, the structure of our AppM will automatically handle it for us.

newtype AppM a = AppM (Env -> IO (Either Error a))
  deriving Functor

-- The runAppM function only needs to change the final return type as it has an
-- 'Either Error' and not just the 'a'.
runAppM
  :: AppM a
  -> Env
  -> IO (Either Error a)
runAppM (AppM m) =
  m

-- Copy over your previously completed definitions.

instance Applicative AppM where
  pure :: a -> AppM a
  pure  = AppM . const . pure . pure

  (<*>) :: AppM (a -> b) -> AppM a -> AppM b
  (<*>) (AppM f) (AppM a) =
    -- f :: Env -> IO (Either Error (a -> b))
    -- a :: Env -> IO (Either Error a)
    AppM $ liftA2 (liftA2 (<*>)) f a

instance Monad AppM where
  return :: a -> AppM a
  return = pure

  (>>=) :: forall a b . AppM a -> (a -> AppM b) -> AppM b
  (>>=) (AppM a) f =
    let
      g e = ($ e) . runAppM . f
      f' e = either (pure . Left) (g e)
    in
      AppM $ liftA2 (>>=) a f'

instance MonadIO AppM where
  liftIO :: IO a -> AppM a
  liftIO =
    AppM . const . fmap pure

instance MonadReader Env AppM where
  ask :: AppM Env
  ask = AppM (pure . pure)

  local :: (Env -> Env) -> AppM a -> AppM a
  local f (AppM a) =
    AppM $ a . f

  reader :: (Env -> a) -> AppM a
  reader f =
    AppM $ pure . pure . f

instance MonadError Error AppM where
  throwError :: Error -> AppM a
  throwError = AppM . const . pure . Left

  catchError :: AppM a -> (Error -> AppM a) -> AppM a
  catchError (AppM a) f =
    AppM $ \e -> a e >>= either (($ e) . runAppM . f) (pure . Right)

-- This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
liftEither
  :: Either Error a
  -> AppM a
liftEither =
  AppM . const . pure
