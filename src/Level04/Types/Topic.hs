module Level04.Types.Topic
  ( Topic
  , mkTopic
  , getTopic
  ) where

import           Data.Aeson                 (ToJSON (..))

import           Data.Functor.Contravariant (contramap)
import           Data.Text                  (Text)

import           Level04.Types.Error        (Error (EmptyTopic), nonEmptyText)

newtype Topic = Topic Text
  deriving Show

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

-- | We're going to write the JSON encoder for our `Topic` type. We'll need to consult the
-- documentation in the 'Aeson' package to find the relevant functions and instructions on how to
-- use them:
--
-- 'https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html'
--
instance ToJSON Topic where
  toJSON = error "Topic ToJSON instance no implemented"
