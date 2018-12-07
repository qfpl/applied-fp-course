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
  , encodeComment
  , encodeTopic
  ) where

import           GHC.Generics                       (Generic)
import           GHC.Word                           (Word16)

import           Data.ByteString                    (ByteString)
import           Data.Text                          (Text, pack)

import           System.IO.Error                    (IOError)

import           Data.Functor.Contravariant         ((>$<))
import           Data.Monoid                        (Last,
                                                     Monoid (mappend, mempty))
import           Data.Semigroup                     (Semigroup ((<>)))

import           Data.List                          (stripPrefix)
import           Data.Maybe                         (fromMaybe)
import           Data.Time                          (UTCTime)
import qualified Data.Time.Format                   as TF

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Waargonaut.Encode                  (Encoder)
import qualified Waargonaut.Encode                  as E

import           Level05.DB.Types                   (DBComment (dbCommentComment, dbCommentId, dbCommentTime, dbCommentTopic))

import           Level05.Types.CommentText          (CommentText,
                                                     encodeCommentText,
                                                     getCommentText,
                                                     mkCommentText)
import           Level05.Types.Error                (Error (DBError, EmptyCommentText, EmptyTopic, UnknownRoute))
import           Level05.Types.Topic                (Topic, encodeTopic,
                                                     getTopic, mkTopic)

newtype CommentId = CommentId Int
  deriving (Show)

encodeCommentId :: Applicative f => Encoder f CommentId
encodeCommentId = (\(CommentId i) -> i) >$< E.int

data Comment = Comment
  { commentId    :: CommentId
  , commentTopic :: Topic
  , commentText  :: CommentText
  , commentTime  :: UTCTime
  }
  deriving Show

encodeISO8601DateTime :: Applicative f => Encoder f UTCTime
encodeISO8601DateTime = pack . TF.formatTime tl fmt >$< E.text
  where
    fmt = TF.iso8601DateFormat (Just "%H:%M:%S")
    tl = TF.defaultTimeLocale { TF.knownTimeZones = [] }

encodeComment :: Applicative f => Encoder f Comment
encodeComment = E.mapLikeObj $ \c ->
  E.atKey' "id"    encodeCommentId       (commentId c) .
  E.atKey' "topic" encodeTopic           (commentTopic c) .
  E.atKey' "text"  encodeCommentText     (commentText c) .
  E.atKey' "time"  encodeISO8601DateTime (commentTime c)

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
