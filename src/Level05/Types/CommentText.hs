{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Level05.Types.CommentText
  ( CommentText
  , mkCommentText
  , getCommentText
  ) where

import           Data.Aeson          (ToJSON)
import           Data.Text           (Text)

import           Level05.Types.Error (Error (EmptyCommentText), nonEmptyText)

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
