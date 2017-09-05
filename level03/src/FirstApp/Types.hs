{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Types
  ( Error (..)
  , RqType (..)
  , ContentType (..)
  , Topic
  , CommentText
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  ) where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)

newtype Topic = Topic Text
  deriving Show

newtype CommentText = CommentText Text
  deriving Show

mkTopic
  :: Text
  -> Either Error Topic
mkTopic "" =
  Left EmptyTopic
mkTopic ti =
  Right (Topic ti)

getTopic
  :: Topic
  -> Text
getTopic (Topic t) =
  t

mkCommentText
  :: Text
  -> Either Error CommentText
mkCommentText "" =
  Left EmptyCommentText
mkCommentText ct =
  Right (CommentText ct)

getCommentText
  :: CommentText
  -> Text
getCommentText (CommentText t) =
  t

-- We have to be able to:
-- - Comment on a given topic
-- - View a topic and its comments
-- - List the current topics
--
-- To that end, we have the following types:
--
-- AddRq : Which needs to the target topic, and the body of the comment.
-- ViewRq : Which needs the topic being requested.
-- ListRq : Which lists all of the current topics.
data RqType
  = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  deriving Show

data ContentType
  = PlainText
  | JSON

-- The ContentType description for a header doesn't match our data definition so
-- we write a little helper function to pattern match on our ContentType value
-- and provide the correct header value.
renderContentType
  :: ContentType
  -> ByteString
renderContentType PlainText = "text/plain"
renderContentType JSON      = "application/json"
