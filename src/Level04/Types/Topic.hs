{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Level04.Types.Topic
  ( Topic
  , mkTopic
  , getTopic
  ) where

import           Level04.Types.Error (Error (EmptyTopic), nonEmptyText)

import           Data.Aeson          (ToJSON)
import           Data.Text           (Text)

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
