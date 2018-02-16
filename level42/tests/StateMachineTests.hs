module StateMachineTests where

import qualified Data.IntMap    as M
import           Data.Text      (Text)

import           Hedgehog       (Command)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

import           FirstApp.Types (Conf (Conf), DBFilePath (DBFilePath),
                                 Port (Port))

main :: IO ()
main =
  putStrLn "Hello, State Machine Tests"

data Comment =
  Comment { topic   :: Text
          , comment :: Text
          } deriving (Eq, Show)

type CommentState = M.IntMap Comment

config :: Conf
config = Conf (Port 3000) (DBFilePath "state-machine-tests.sqlite")

initialState :: CommentState
initialState =
  M.empty
