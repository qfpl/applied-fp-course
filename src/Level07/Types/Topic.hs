module Level07.Types.Topic
  ( Topic
  , mkTopic
  , getTopic
  ) where

import           Data.Aeson          (ToJSON (..))

import           Data.Text           (Text)

import           Level07.Types.Error (Error (EmptyTopic), nonEmptyText)

newtype Topic = Topic Text
  deriving (Show)

instance ToJSON Topic where
  toJSON (Topic t) = toJSON t

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
