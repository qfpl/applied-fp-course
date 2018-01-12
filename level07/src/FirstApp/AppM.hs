{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FirstApp.AppM where

import           Control.Monad.Except   (ExceptT, MonadError, runExceptT,
                                         throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)

import           FirstApp.Conf          (Conf)
import           FirstApp.DB.Types      (FirstAppDB)
import           FirstApp.Error         (Error)

data Env = Env
  { envConfig :: Conf
  , envDb     :: FirstAppDB
  }

-- We're going to add a very nice piece to our application, in the form of
-- "automating" away our explicit error handling without losing the valuable
-- type level information that things may go awry.
--
-- To do this we will expand the capabilities of our AppM with another monad
-- transformer, ExceptT. This is an error handling monad that will immediately
-- halt and return when an error is 'thrown'. Note that this does not prevent us
-- from returning 'Either' values when we need to.
--
-- ExceptT e m a ~ m (Either e a)
--
-- Notice that ExceptT is parametrised over the `e` in same way that our
-- ReaderT is parametrised over the `r`.
--
-- ReaderT r m a ~ r -> m a
--
-- This transformer operates in the same manner as the Functor/Applicative/Monad
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
-- The ExceptT takes this capability and extends it to functions that return a
-- type of `m (Either e a)`. Which is in fact the type of the ExceptT
-- constructor!
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
-- We can wrap our functions with ExceptT and we can work directly with the
-- values we expect to appear on the happy path, knowing that if the sad path is
-- encountered, the structure of the monad will automatically handle it for us.
-- We then `runExceptT` similar to how we `runReaderT` and the final value is an
-- `Either e a` that we can handle once, instead of at each and every place an
-- error may occur:
--
-- foo :: ExceptT Error IO Value
-- foo = do
--  a <- mightFail'
--  alsoMightFail' a
--  where
--    mightFail' :: ExceptT Error IO Int
--    mightFail' = ExceptT mightFail
--
--    alsoMightFail' :: Int -> ExceptT Error IO Value
--    alsoMightFail' = ExceptT . alsoMightFail
--
-- ( runExceptT foo ) :: IO (Either Error Value)
--
-- But how to extend our AppM ?
--
-- We could change to use the ExceptT over IO, but we would lose the
-- functionality provided by the ReaderT.
--
-- Recall that the type of the ReaderT r m a ~ r -> m a
--
-- ReaderT is a monad transformer over or for some base `m`, our initial use
-- case was to transform over `IO` as our base.
--
-- This polymorphism allows us to 'stack' our monad transformers, effectively
-- combining the functionality of the different transformer types into a single
-- monad.
--
-- But ExceptT has an `m` type variable, so what do we put there?
--
newtype AppM a = AppM
  { unAppM :: ReaderT Env (ExceptT Error IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Env
           -- This is a new instance we've derived because we want to be able to
           -- `throwError` our own Error type.
           , MonadError Error
           )

-- Now that we've two transformers in our 'stack', we have to 'run' them in order
-- to effect the computations and produce our result in our base monad. When
-- you're running monad transformers you have to unpack them in order. Since our outer
-- transformer is a ReaderT, we have to run that first. Followed by running
-- the ExceptT to retrieve our `IO (Either Error a)`.
runAppM
  :: Env
  -> AppM a
  -> IO (Either Error a)
runAppM =
  error "runAppM not reimplemented"

-- This is a helper function that will `lift` an Either value into our new AppM
-- by applying `throwError` to the Left value, and using `pure` to lift the
-- Right value into the AppM.
--
-- throwError :: MonadError e m => e -> m a
-- pure :: Applicative m => a -> m a
--
throwL
  :: Either Error a
  -> AppM a
throwL =
  error "throwL not implemented"
