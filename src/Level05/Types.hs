{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Level05.Types
  ( Error (..)
  , RqType (..)
  , ContentType (..)
  , Comment (..)
  , Topic
  , CommentText
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  , fromDBComment
  ) where

import           GHC.Generics                       (Generic)
import           GHC.Word                           (Word16)

import           Data.ByteString                    (ByteString)
import           Data.Text                          (Text, pack)

import           Data.Aeson                         (ToJSON (..), object, (.=))
import           Data.Monoid                        (Last,
                                                     Monoid (mappend, mempty))
import           Data.Semigroup                     (Semigroup ((<>)))
import           System.IO.Error                    (IOError)

import           Data.List                          (stripPrefix)
import           Data.Maybe                         (fromMaybe)
import           Data.Time                          (UTCTime)
import qualified Data.Time.Format                   as TF

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level05.DB.Types                   (DBComment (dbCommentComment, dbCommentId, dbCommentTime, dbCommentTopic))

import           Level05.Types.CommentText          (CommentText,
                                                     getCommentText,
                                                     mkCommentText)
import           Level05.Types.Error                (Error (DBError, EmptyCommentText, EmptyTopic, UnknownRoute))
import           Level05.Types.Topic                (Topic, getTopic, mkTopic)

newtype CommentId = CommentId Int
  deriving (Show)

instance ToJSON CommentId where
  toJSON (CommentId i) = toJSON i

data Comment = Comment
  { commentId    :: CommentId
  , commentTopic :: Topic
  , commentText  :: CommentText
  , commentTime  :: UTCTime
  }
  deriving Show

instance ToJSON Comment where
  toJSON c = object
    [ "id" .= (commentId c)
    , "topic" .= (commentTopic c)
    , "text" .= (commentText c)
    , "time" .= (commentTime c)
    ]

-- For safety we take our stored DBComment and try to construct a Comment that
-- we would be okay with showing someone. However unlikely it may be, this is a
-- nice method for separating out the back and front end of a web app and
-- providing greater guarantees about data cleanliness.

fromDBComment
  :: DBComment
  -> Either Error Comment
fromDBComment dbc =
  Comment (CommentId     $ dbCommentId dbc)
      <$> (mkTopic       $ dbCommentTopic dbc)
      <*> (mkCommentText $ dbCommentComment dbc)
      <*> (pure          $ dbCommentTime dbc)

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

data ContentType
  = PlainText
  | JSON

renderContentType
  :: ContentType
  -> ByteString
renderContentType PlainText = "text/plain"
renderContentType JSON      = "application/json"
