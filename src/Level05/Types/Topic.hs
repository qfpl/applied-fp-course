{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Level05.Types.Topic
  ( Topic
  , mkTopic
  , getTopic
  ) where

import           Data.Aeson          (ToJSON)
import           Data.Text           (Text)

import           Level05.Types.Error (Error (EmptyTopic), nonEmptyText)

newtype Topic = Topic Text
  deriving (Show, ToJSON)

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
