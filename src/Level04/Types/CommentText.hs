{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Level04.Types.CommentText
  ( CommentText
  , mkCommentText
  , getCommentText
  ) where

import           Level04.Types.Error (Error (EmptyCommentText), nonEmptyText)

import           Data.Aeson          (ToJSON)
import           Data.Text           (Text)

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
