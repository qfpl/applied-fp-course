{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative       (liftA3)
import           Control.Exception         (catch, throw)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Morph       (hoist)
import           Control.Monad.Trans.Class (lift)
import           Data.Bool                 (bool)
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.IntMap               as M
import           Data.Semigroup            ((<>))
import           Data.Text                 (Text)
import           Data.Text.IO              (hPutStrLn)
import           GHC.Word                  (Word16)
import qualified Network.Wai.Test          as WT
import           System.Directory          (removeFile)
import           System.IO                 (stderr)
import           System.IO.Error           (isDoesNotExistError)

import           Hedgehog                  (Callback (..), Command (Command),
                                            Gen, HTraversable (htraverse),
                                            Property, PropertyT, assert,
                                            executeSequential, forAll, property)
import qualified Hedgehog.Gen              as Gen
import qualified Hedgehog.Range            as Range
import           Test.Tasty                (defaultMain)
import           Test.Tasty.Hedgehog       (testProperty)

import           FirstApp.AppM             (Env (Env))
import           FirstApp.DB               (initDB)
import           FirstApp.Main             (app)
import           FirstApp.Types            (Conf (Conf),
                                            DBFilePath (DBFilePath),
                                            Port (Port), dbFilePath)

main :: IO ()
main =
  defaultMain . testProperty "FirstApp" $ propFirstApp

portNum :: Word16
portNum = 3000

data Comment =
  Comment { topic   :: Text
          , comment :: Text
          } deriving (Eq, Show)

newtype CommentState (v :: * -> *) =
  CommentState (M.IntMap Comment)
  deriving (Eq, Show)

env :: IO Env
env =
  let
    dbPath = "state-machine-tests.sqlite"
    c = Conf (Port 3000) (DBFilePath dbPath)
    edb = initDB (dbFilePath c)
    logErr = liftIO . hPutStrLn stderr
    splode = error . ("Error connecting to DB: " <>) . show
    checkRmException =
      liftA3 bool throw (const . putStrLn $ "No test DB to delete") isDoesNotExistError
  in
    removeFile dbPath `catch` checkRmException
      >> fmap (either splode (Env logErr c)) edb

initialState :: CommentState v
initialState = CommentState M.empty

data ListTopics (v :: * -> *) =
  ListTopics
  deriving (Eq, Show)

instance HTraversable ListTopics where
  htraverse _ ListTopics = pure ListTopics

cListTopicsEmpty :: Command Gen (PropertyT WT.Session) CommentState
cListTopicsEmpty =
  let
    gen :: CommentState v -> Maybe (Gen (ListTopics v))
    gen (CommentState s)
      | s == M.empty = Just (pure ListTopics)
      | otherwise    = Nothing

    execute ListTopics = do
      rsp <- lift $ WT.request . WT.setPath WT.defaultRequest $ "/list"
      pure $ WT.simpleBody rsp

    callbacks =
      [ Require (\(CommentState s) _i -> M.null s)
      , Ensure (\_b _a _i -> assert . (== "[]"))
      ]
  in
    Command gen execute callbacks

propFirstApp :: Property
propFirstApp =
  property $ do
    env' <- liftIO env
    commands <- forAll $
      Gen.sequential (Range.linear 1 100) initialState [cListTopicsEmpty]
    let session :: PropertyT WT.Session ()
        session =  executeSequential initialState commands

    hoist (`WT.runSession` app env') session
