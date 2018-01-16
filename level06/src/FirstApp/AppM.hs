{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FirstApp.AppM where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)

import           Data.Text              (Text)

import           FirstApp.Types         (Conf, FirstAppDB)

-- a ReaderT is a function from some 'r' to some 'm a' : (r -> m a). Whereby
-- the 'r' is accessible to all functions that run in the context of that 'm'.
--
-- This means that if you use the 'r' everywhere or enough throughout your
-- application, you no longer have to constantly weave the extra 'r' as an
-- argument to everything that might need it.
-- Since by definition:
-- foo :: ReaderT r m a
-- When run, becomes:
-- foo :: r -> m a
--
-- First, let's clean up our (Conf,FirstAppDB) with an application Env type. We
-- will add a general purpose logging function as well
data Env = Env
  { envLoggingFn :: Text -> AppM ()
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

-- Lets crack on and define a newtype wrapper for our ReaderT, this will save us
-- having to write out the full ReaderT definition for every function that uses
-- it.

-- Our ReaderT will only contain the Env, and our base monad will be IO, leave
-- the return type polymorphic so that it will work regardless of what is
-- being returned from the functions that will use it. Using a newtype (in
-- addition to the useful type system) means that it is harder to use a
-- different ReaderT when we meant to use our own, or vice versa. In such a
-- situation it is extremely unlikely the application would compile at all,
-- but the name differences alone make the confusion less likely.

-- Because we're using a newtype, all of the instance definitions for ReaderT
-- would normally not apply. However, because we've done nothing but create a
-- convenience wrapper for our ReaderT, it is not difficult for GHC to
-- automatically derive instances on our behalf.

-- With the 'GeneralizedNewtypeDeriving' pragma at the top of the file, we
-- will be able to derive these instances automatically.
newtype AppM a = AppM
  { unAppM :: ReaderT Env IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Env
           , MonadIO
           )

-- Below is a helper function that will take the requirements for our ReaderT,
-- an Env, and the (AppM a) that is the action to be run with the given Env.

-- Our AppM must first be 'unwrapped', the newtype definition we wrote gives us
-- that function:
-- > unAppM :: AppM a -> ReaderT Env IO a
--
-- Then we run the ReaderT, using:
-- > runReaderT :: ReaderT r m a -> r -> m a
-- ~
-- > runReaderT :: ReaderT Env IO a -> Env -> IO a
--
-- Composing them (runReaderT . unAppM) we are left with:
-- > Env -> IO a
--
-- We have an Env so that leaves us with the:
-- > IO a
-- and we're done.
runAppM
  :: Env
  -> AppM a
  -> IO a
runAppM =
  error "runAppM not implemented"

-- Move on to ``src/FirstApp/DB.hs`` after this
