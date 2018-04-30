{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Level06.Types.Topic (Topic, mkTopic, getTopic) where

import Level06.Types.Error (Error(EmptyTopic), nonEmptyText)

import Data.Text (Text)
import Data.Aeson (ToJSON)

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
