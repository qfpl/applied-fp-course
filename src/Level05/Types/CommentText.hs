module Level05.Types.CommentText
  ( CommentText
  , mkCommentText
  , getCommentText
  ) where

import           Level05.Types.Error (Error (EmptyCommentText), nonEmptyText)

import           Data.Aeson          (ToJSON (..))

import           Data.Text           (Text)

newtype CommentText = CommentText Text
  deriving (Show)

instance ToJSON CommentText where
  toJSON (CommentText t) = toJSON t

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
