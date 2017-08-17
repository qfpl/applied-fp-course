{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FirstApp.AppM where

import           Control.Monad.Except   (ExceptT, MonadError, runExceptT, throwError)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)

import           FirstApp.Conf          (Conf)
import           FirstApp.DB.Types      (FirstAppDB)
import           FirstApp.Error         (Error)

data Env = Env
  { envConfig :: Conf
  , envDb     :: FirstAppDB
  }

newtype AppM a = AppM
  { unAppM :: ReaderT Env (ExceptT Error IO) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Env
           , MonadError Error
           , MonadIO
           )

runAppM
  :: Env
  -> AppM a
  -> IO (Either Error a)
runAppM env appM =
  runExceptT ( runReaderT (unAppM appM) env )

throwL
  :: Either Error a
  -> AppM a
throwL =
  either throwError pure
