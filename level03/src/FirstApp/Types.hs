{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Types
  ( Error (..)
  , RqType (..)
  , ContentType (..)
  -- Exporting newtypes like this will hide the constructor.
  , Topic (getTopic)
  , CommentText (getCommentText)
  -- We provide specific constructor functions.
  , mkTopic
  , mkCommentText
  , renderContentType
  ) where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)

newtype Topic = Topic
  { getTopic :: Text }
  deriving Show

newtype CommentText = CommentText
  { getCommentText :: Text }
  deriving Show

-- Having specialised constructor functions for the newtypes allows you to set
-- restrictions for your newtype.
mkTopic
  :: Text
  -> Either Error Topic
mkTopic "" =
  Left EmptyTopic
mkTopic ti =
  Right (Topic ti)

mkCommentText
  :: Text
  -> Either Error CommentText
mkCommentText "" =
  Left EmptyCommentText
mkCommentText ct =
  Right (CommentText ct)

-- We have to be able to:
-- - Comment on a given topic
-- - View a topic and its comments
-- - List the current topics
--
-- To that end, we have the following types:
--
-- AddRq : Which needs to the target topic, and the body of the comment.
-- ViewRq : Which needs the topic being requestd.
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

-- The ContentType description for a header doesn't match our data definition
-- so we write a little helper function to pattern match on our ContentType
-- value and provide the correct header value.
renderContentType
  :: ContentType
  -> ByteString
renderContentType PlainText = "text/plain"
renderContentType JSON      = "text/json"
