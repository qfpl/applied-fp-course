{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module FirstApp.AppM where

import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..))

import           Data.Text              (Text)

import           FirstApp.Types         (Conf, FirstAppDB)

-- First, let's clean up our (Conf,FirstAppDB) with an application Env type. We
-- will add a general purpose logging function as well. Remember that functions
-- are values, we're able to pass them around and place them on records like any
-- other type.
data Env = Env
  { envLoggingFn :: Text -> AppM ()
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

-- It would be nice to remove the need to pass around our Env to every function
-- that needs it. Wouldn't it be great to have our functions run where we could
-- simply ask for the current Env?
--
-- We can create this by wrapping a function in a newtype like so:
newtype AppM a = AppM ( Env -> IO a )
-- This means that any function where we want to use the Env will now return
-- this data structure. Which is a function that needs an Env. We can then write
-- a `Monad` instance for this type that we use to pass the Env from function to
-- function.

runAppM
  :: AppM a
  -> Env
  -> IO a
runAppM =
  error "runAppM not implemented"

instance Functor AppM where
  fmap :: (a -> b) -> AppM a -> AppM b
  fmap = error "fmap for AppM not implemented"

instance Applicative AppM where
  pure :: a -> AppM a
  pure = error "pure for AppM not implemented"

  (<*>) :: AppM (a -> b) -> AppM a -> AppM b
  (<*>) = error "ap for AppM not implemented"

instance Monad AppM where
  return :: a -> AppM a
  return = error "return for AppM not implemented"

  (>>=) :: AppM a -> (a -> AppM b) -> AppM b
  (>>=) = error "bind for AppM not implemented"

instance MonadReader Env AppM where
  ask :: AppM Env
  ask = error "ask for AppM not implemented"

  local :: (Env -> Env) -> AppM a -> AppM a
  local = error "local for AppM not implemented"

  reader :: (Env -> a) -> AppM a
  reader = error "reader for AppM not implemented"

instance MonadIO AppM where
  liftIO :: IO a -> AppM a
  liftIO = error "liftIO for AppM not implemented"

-- Move on to ``src/FirstApp/DB.hs`` after this
