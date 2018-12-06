module Level06.Types.Topic
  ( Topic
  , mkTopic
  , getTopic
  , encodeTopic
  ) where

import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E

import           Level06.Types.Error        (Error (EmptyTopic), nonEmptyText)

import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)

newtype Topic = Topic Text
  deriving Show

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
