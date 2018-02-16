module StateMachineTests where

import           Control.Concurrent.Async (race_)
import           Control.Concurrent.MVar  (MVar, newEmptyMVar, putMVar,
                                           takeMVar)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.IntMap              as M
import           Data.Semigroup           ((<>))
import           Data.Text                (Text)
import           Data.Text.IO             (hPutStrLn)
import           GHC.Word                 (Word16)
import           Network.Wai.Handler.Warp (run)
import           System.IO                (stderr)

import           Hedgehog                 (Command)
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

type CommentState = M.IntMap Comment

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

initialState :: CommentState
initialState =
  M.empty
