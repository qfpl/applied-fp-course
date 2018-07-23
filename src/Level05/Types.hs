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
import           Data.Text                          (Text)

import           System.IO.Error                    (IOError)

import           Data.Monoid                        (Last,
                                                     Monoid (mappend, mempty))
import           Data.Semigroup                     (Semigroup ((<>)))

import           Data.List                          (stripPrefix)
import           Data.Maybe                         (fromMaybe)
import           Data.Time                          (UTCTime)

import           Data.Aeson                         (FromJSON (..), ToJSON)
import qualified Data.Aeson                         as A
import qualified Data.Aeson.Types                   as A

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import           Level05.DB.Types                   (DBComment (dbCommentComment, dbCommentId, dbCommentTime, dbCommentTopic))
import           Level05.Types.CommentText          (CommentText,
                                                     getCommentText,
                                                     mkCommentText)
import           Level05.Types.Error                (Error (DBError, EmptyCommentText, EmptyTopic, UnknownRoute))
import           Level05.Types.Topic                (Topic, getTopic, mkTopic)

newtype CommentId = CommentId Int
  deriving (Show, ToJSON)

data Comment = Comment
  { commentId    :: CommentId
  , commentTopic :: Topic
  , commentText  :: CommentText
  , commentTime  :: UTCTime
  }
  -- Generic has been added to our deriving list.
  deriving ( Show, Generic )

-- Strip the prefix (which may fail if the prefix isn't present), fall
-- back to the original label if need be, then camel-case the name.

-- | modFieldLabel
-- >>> modFieldLabel "commentId"
-- "id"
-- >>> modFieldLabel "topic"
-- "topic"
-- >>> modFieldLabel ""
-- ""
modFieldLabel
  :: String
  -> String
modFieldLabel l =
  A.camelTo2 '_'
  . fromMaybe l
  $ stripPrefix "comment" l

instance ToJSON Comment where
  -- This is one place where we can take advantage of our Generic instance. Aeson
  -- already has the encoding functions written for anything that implements the
  -- Generic typeclass. So we don't have to write our encoding, we tell Aeson to
  -- build it.
  toEncoding = A.genericToEncoding opts
    where
      -- These options let us make some minor adjustments to how Aeson treats
      -- our type. Our only adjustment is to alter the field names a little, to
      -- remove the 'comment' prefix and use an Aeson function to handle the
      -- rest of the name. This accepts any 'String -> String' function but it's
      -- wise to keep the modifications simple.
      opts = A.defaultOptions
             { A.fieldLabelModifier = modFieldLabel
             }

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
