{-# LANGUAGE DeriveGeneric #-}

module Level04.Types.CommentText (
  CommentText,
  mkCommentText,
  getCommentText,
) where

import Level04.Types.Error (
  Error (EmptyCommentText),
  nonEmptyText,
 )

import Data.Aeson (ToJSON)
import Data.Functor.Contravariant (contramap)
import Data.Text (Text)
import Database.SQLite.Simple.ToField
import GHC.Generics (Generic)

newtype CommentText = CommentText Text
  deriving (Show, Generic)

mkCommentText ::
  Text ->
  Either Error CommentText
mkCommentText =
  nonEmptyText CommentText EmptyCommentText

getCommentText ::
  CommentText ->
  Text
getCommentText (CommentText t) =
  t

-- Samme som topic
instance ToJSON CommentText

instance ToField CommentText where
  toField (CommentText t) = toField t