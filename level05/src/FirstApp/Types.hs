{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module FirstApp.Types
  ( Error (..)
  , RqType (..)
  , ContentType (..)
  -- Exporting newtypes like this will hide the constructor.
  , Topic (getTopic)
  , CommentText (getCommentText)
  , Comment (..)
  -- We provide specific constructor functions.
  , mkTopic
  , mkCommentText
  , renderContentType
  , fromDbComment
  ) where

import           GHC.Generics      (Generic)

import           Data.ByteString   (ByteString)
import           Data.Text         (Text)

import           Data.List         (stripPrefix)
import           Data.Maybe        (fromMaybe)

import           Data.Aeson        (ToJSON (..))
import qualified Data.Aeson        as A
import qualified Data.Aeson.Types  as A

import           Data.Time         (UTCTime)

import           FirstApp.Types.DB (DbComment (..))

{-|
In Haskell the `newtype` comes with zero runtime cost. It is purely used for
typechecking. So when you have a bare 'primitive' value, like an Int, String, or
even [a], you can wrap it up in a `newtype` for clarity.

The type system will check it for you, and the compiler will eliminate the cost
once it has passed.
-}
newtype CommentId = CommentId Int
  deriving (Show, ToJSON)

newtype Topic = Topic { getTopic :: Text }
  deriving (Show, ToJSON)

newtype CommentText = CommentText { getCommentText :: Text }
  deriving (Show, ToJSON)

data Comment = Comment
  { commentId    :: CommentId
  , commentTopic :: Topic
  , commentText  :: CommentText
  , commentTime  :: UTCTime
  }
  deriving ( Show, Generic )

instance ToJSON Comment where
  toEncoding = A.genericToEncoding opts
    where
      opts = A.defaultOptions
             { A.fieldLabelModifier = modFieldLabel
             }

      modFieldLabel l =
        fromMaybe l $ stripPrefix "comment" l

fromDbComment
  :: DbComment
  -> Either Error Comment
fromDbComment dbc =
  Comment (CommentId $ dbCommentId dbc)
    <$> (mkTopic $ dbCommentTopic dbc)
    <*> (mkCommentText $ dbCommentComment dbc)
    <*> pure (dbCommentTime dbc)

-- Having specialised constructor functions for the newtypes allows you to set
-- restrictions for your newtype.
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

mkCommentText
  :: Text
  -> Either Error CommentText
mkCommentText =
  nonEmptyText CommentText EmptyCommentText

data RqType
  = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

{-|
Not everything goes according to plan, but it's important that our
types reflect when errors can be introduced into our program. Additionally
it's useful to be able to be descriptive about what went wrong.

So lets think about some of the basic things that can wrong with our
program and create some values to represent that.
-}
data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  | DBError Text
  deriving Show

-- Provide a type to list our response content types so we don't try to
-- do the wrong thing with what we meant to be used as text/JSON etc.
data ContentType
  = PlainText
  | JSON

-- The ContentType description for a header doesn't match our data definition
-- so we write a little helper function to pattern match on our ContentType
-- value and provide the correct header value.
renderContentType
  :: ContentType
  -> ByteString
-- renderContentType = error "renderContentType not implemented"
renderContentType PlainText = "text/plain"
renderContentType JSON      = "text/json"
