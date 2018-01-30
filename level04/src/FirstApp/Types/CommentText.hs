{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FirstApp.Types.CommentText
  ( CommentText
  , mkCommentText
  , getCommentText
  ) where

import FirstApp.Types.Error (Error(EmptyCommentText), nonEmptyText)

import Data.Text (Text)
import Data.Aeson (ToJSON)

newtype CommentText = CommentText Text
  deriving (Show, ToJSON)

mkCommentText
  :: Text
  -> Either Error CommentText
mkCommentText =
  nonEmptyText CommentText EmptyCommentText

getCommentText
  :: CommentText
  -> Text
getCommentText (CommentText t) =
  t
