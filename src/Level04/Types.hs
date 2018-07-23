{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Level04.Types
  ( Error (..)
  , RqType (..)
  , ContentType (..)
  , Topic
  , CommentText
  , Comment (..)
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  , fromDBComment
  ) where

import           GHC.Generics              (Generic)

import           Data.ByteString           (ByteString)
import           Data.Text                 (Text)

import           Data.List                 (stripPrefix)
import           Data.Maybe                (fromMaybe)

import           Data.Aeson                (ToJSON (toJSON))
import qualified Data.Aeson                as A
import qualified Data.Aeson.Types          as A

import           Data.Time                 (UTCTime)

import           Level04.DB.Types          (DBComment)

-- Notice how we've moved these types into their own modules. It's cheap and
-- easy to add modules to carve out components in a Haskell application. So
-- whenever you think that a module is too big, covers more than one piece of
-- distinct functionality, or you want to carve out a particular piece of code,
-- just spin up another module.
import           Level04.Types.CommentText (CommentText, getCommentText, mkCommentText)
import           Level04.Types.Error       (Error (EmptyCommentText, EmptyTopic, UnknownRoute))
import           Level04.Types.Topic       (Topic, getTopic, mkTopic)


-- This is the `Comment` record that we will be sending to users, it's a simple
-- record type, containing an `Int`, `Topic`, `CommentText`, and `UTCTime`.
-- However notice that we've also derived the `Generic` type class instance as
-- well. This saves us some effort when it comes to creating encoding/decoding
-- instances. Since our types are all simple types at the end of the day, we're
-- able to let GHC do the work.
newtype CommentId = CommentId Int
  deriving (Eq, Show, ToJSON)

data Comment = Comment
  { commentId    :: CommentId
  , commentTopic :: Topic
  , commentBody  :: CommentText
  , commentTime  :: UTCTime
  }
  deriving ( Show, Generic )

-- Strip the prefix, or fall back to the original label if prefix not present.

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
modFieldLabel =
  error "modFieldLabel not implemented"

instance ToJSON Comment where
  -- This is one place where we can take advantage of our `Generic` instance.
  -- Aeson already has the encoding functions written for anything that
  -- implements the `Generic` typeclass. So we don't have to write our encoding,
  -- we ask Aeson to construct it for us.
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

-- For safety we take our stored `DBComment` and try to construct a `Comment`
-- that we would be okay with showing someone. However unlikely it may be, this
-- is a nice method for separating out the back and front end of a web app and
-- providing greater guarantees about data cleanliness.
fromDBComment
  :: DBComment
  -> Either Error Comment
fromDBComment =
  error "fromDBComment not yet implemented"

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
