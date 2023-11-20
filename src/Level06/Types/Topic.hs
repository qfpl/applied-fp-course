module Level06.Types.Topic
  ( Topic
  , mkTopic
  , getTopic
  ) where

import           Data.Aeson          (ToJSON (..))

import           Level06.Types.Error (Error (EmptyTopic), nonEmptyText)

import           Data.Text           (Text)

newtype Topic = Topic Text
  deriving Show

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
