module Level06.Types.CommentText
  ( CommentText
  , mkCommentText
  , getCommentText
  ) where

import           Data.Aeson          (ToJSON (..))

import           Level06.Types.Error (Error (EmptyCommentText), nonEmptyText)

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
