module Level07.Types.Topic
  ( Topic
  , mkTopic
  , getTopic
  , encodeTopic
  ) where

import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E

import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)

import           Level07.Types.Error        (Error (EmptyTopic), nonEmptyText)

newtype Topic = Topic Text
  deriving (Show)

encodeTopic :: Applicative f => Encoder f Topic
encodeTopic = getTopic >$< E.text

mkTopic
  :: Text
  -> Either Error Topic
mkTopic =
  nonEmptyText Topic EmptyTopic

getTopic
  :: Topic
  -> Text
getTopic (Topic t) =
  t
