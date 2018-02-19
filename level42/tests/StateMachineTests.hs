{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module StateMachineTests where

import           Control.Concurrent.Async (race_)
import           Control.Concurrent.MVar  (MVar, newEmptyMVar, putMVar,
                                           takeMVar)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.IntMap              as M
import           Data.Semigroup           ((<>))
import           Data.Text                (Text)
import           Data.Text.IO             (hPutStrLn)
import           GHC.Word                 (Word16)
import           Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Test         as WT
import           System.IO                (stderr)

import           Hedgehog                 (executeSequential, Callback (..), Command (Command),
                                           Gen, HTraversable (htraverse),
                                           Property, assert, forAll, property)
import qualified Hedgehog.Gen             as Gen
import qualified Hedgehog.Range           as Range

import           FirstApp.AppM            (Env (Env))
import           FirstApp.DB              (initDB)
import           FirstApp.Main            (app)
import           FirstApp.Types           (Conf (Conf), DBFilePath (DBFilePath),
                                           Port (Port))

main :: IO ()
main =
  putStrLn "test"

runAppUntilSignalled
  :: MVar ()
  -> IO ()
runAppUntilSignalled m = do
  env' <- env
  let runApp = run (fromIntegral portNum) (app env')
  race_ runApp (takeMVar m)

portNum :: Word16
portNum = 3000

data Comment =
  Comment { topic   :: Text
          , comment :: Text
          } deriving (Eq, Show)

data CommentState (v :: * -> *) =
  CommentState (M.IntMap Comment)
  deriving (Eq, Show)

env :: IO Env
env =
  let
    dbPath = DBFilePath "state-machine-tests.sqlite"
    c = Conf (Port 3000) dbPath
    edb = initDB dbPath
    logErr = liftIO . hPutStrLn stderr
    splode = error . ("Error connecting to DB: " <>) . show
  in
    fmap (either splode (Env logErr c)) edb

initialState :: CommentState v
initialState = CommentState M.empty

data ListTopics (v :: * -> *) =
  ListTopics
  deriving (Eq, Show)

instance HTraversable ListTopics where
  htraverse _ ListTopics = pure ListTopics

cListTopicsEmpty :: Command Gen WT.Session CommentState
cListTopicsEmpty =
  let
    gen :: CommentState v -> Maybe (Gen (ListTopics v))
    gen (CommentState s)
      | s == M.empty = Just (pure ListTopics)
      | otherwise    = Nothing

    execute ListTopics = do
      rsp <- WT.request . WT.setPath WT.defaultRequest $ "/list"
      pure $ WT.simpleBody rsp

    callbacks =
      [ Require (\(CommentState s) _i -> M.null s)
      , Ensure (\_b _a _i -> assert . LBS.null)
      ]
  in
    Command gen execute callbacks

-- propFirstApp :: Property
-- propFirstApp =
--   property $ do
--     env' <- liftIO env
--     commands <- forAll $
--       Gen.sequential (Range.linear 1 100) initialState [cListTopicsEmpty]
--     let session = executeSequential initialState commands
--     liftIO $ WT.runSession session (app env')

