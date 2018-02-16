module StateMachineTests where

import qualified Data.IntMap as M

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

data Comment =
  Comment { topic :: Text
          , comment :: Text
          } deriving (Eq, Show)

type CommentState = M.IntMap Comment
