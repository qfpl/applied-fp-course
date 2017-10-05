{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module FirstApp.Types
  ( RqType (..)
  , ContentType (..)
  , Topic
  , CommentText
  , Comment (..)
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  , fromDbComment
  ) where

import           GHC.Generics      (Generic)

import           Data.ByteString   (ByteString)
import           Data.Text         (Text)

import           Data.List         (stripPrefix)
import           Data.Maybe        (fromMaybe)

import           Data.Aeson        (ToJSON (toJSON))
import qualified Data.Aeson        as A
import qualified Data.Aeson.Types  as A

import           Data.Time         (UTCTime)

import           FirstApp.DB.Types (DbComment (dbCommentComment, dbCommentId, dbCommentTime, dbCommentTopic))

-- The Error type has been moved to a separate module to avoid an import cycle.
import           FirstApp.Error    (Error (EmptyCommentText, EmptyTopic))

newtype CommentId = CommentId Int
  deriving (Show, ToJSON)

newtype Topic = Topic Text
  deriving (Show, ToJSON)

newtype CommentText = CommentText Text
  deriving (Show, ToJSON)

data Comment = Comment
  { commentId    :: CommentId
  , commentTopic :: Topic
  , commentText  :: CommentText
  , commentTime  :: UTCTime
  }
  -- Generic has been added to our deriving list. This will require the
  -- DeriveGeneric pragma to be added to the top of the file
  deriving ( Show, Generic )

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

-- For safety we take our stored DbComment and try to construct a Comment that
-- we would be okay with showing someone. However unlikely it may be, this is a
-- nice method for separating out the back and front end of a web app and
-- providing greater guarantees about data cleanliness.
fromDbComment
  :: DbComment
  -> Either Error Comment
fromDbComment dbc =
  Comment (CommentId     $ dbCommentId dbc)
      <$> (mkTopic       $ dbCommentTopic dbc)
      <*> (mkCommentText $ dbCommentComment dbc)
      <*> pure            (dbCommentTime dbc)

nonEmptyText
  :: (Text -> a)
  -> Error
  -> Text
  -> Either Error a
nonEmptyText _ e "" = Left e
nonEmptyText c _ tx = Right (c tx)

mkTopic
  :: Text
  -> Either Error Topic
mkTopic =
  nonEmptyText Topic EmptyTopic

getTopic
  :: Topic
  -> Text
getTopic (Topic t) =
  t

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

data RqType
  = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

-- Provide a type to list our response content types so we don't try to
-- do the wrong thing with what we meant to be used as text or JSON etc.
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
renderContentType JSON      = "application/json"
