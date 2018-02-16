module StateMachineTests where

import qualified Data.IntMap    as M
import           Data.Text      (Text)

import           Hedgehog       (Command)
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range


main :: IO ()
main =
  putStrLn "Hello, State Machine Tests"

data Comment =
  Comment { topic   :: Text
          , comment :: Text
          } deriving (Eq, Show)

type CommentState = M.IntMap Comment

initialState :: CommentState
initialState =
  M.empty
