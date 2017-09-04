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
  )where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)

{-|
In Haskell the `newtype` comes with zero runtime cost. It is purely used for
typechecking. So when you have a bare 'primitive' value, like an Int, String, or
even [a], you can wrap it up in a `newtype` for clarity.

The type system will check it for you, and the compiler will eliminate the cost
once it has passed.
-}
newtype Topic = Topic Text
  deriving Show

newtype CommentText = CommentText Text
  deriving Show

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
renderContentType JSON      = "application/json"
