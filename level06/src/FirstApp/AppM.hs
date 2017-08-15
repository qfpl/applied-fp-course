{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FirstApp.AppM where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)

import Data.Text (Text)

import           FirstApp.Conf          (Conf)
import           FirstApp.DB            (FirstAppDB)

data Env = Env
  { loggingRick :: Text -> AppM ()
  , envConfig   :: Conf
  , envDb       :: FirstAppDB
  }

newtype AppM a = AppM
  { unAppM :: ReaderT Env IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Env
           , MonadIO
           )

runAppM
  :: Env
  -> AppM a
  -> IO a
runAppM env appM =
  runReaderT (unAppM appM) env

