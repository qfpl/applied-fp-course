{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module FirstApp.Types
  ( Error (..)
  , ConfigError (..)
  , PartialConf (..)
  , Port (..)
  , DBFilePath (..)
  , Conf (..)
  , FirstAppDB (..)
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
  , fromDbComment
  , confPortToWai
  ) where

import           GHC.Generics                       (Generic)
import           GHC.Word                           (Word16)

import           Data.ByteString                    (ByteString)
import           Data.Text                          (Text)

import           Data.List                          (stripPrefix)
import           Data.Maybe                         (fromMaybe)
import           Data.Monoid                        (Last, (<>))

import           Data.Aeson                         (ToJSON)
import qualified Data.Aeson                         as A
import qualified Data.Aeson.Types                   as A

import           Data.Time                          (UTCTime)

import           Database.SQLite.Simple             (Connection)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.DB.Types                  (DbComment (dbCommentComment, dbCommentId, dbCommentTime, dbCommentTopic))

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
      <*> (pure          $ dbCommentTime dbc)


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

data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  -- This is our new error constructor.
  | DBError SQLiteResponse
  deriving Show

-- Provide a type to list our response content types so we don't try to
-- do the wrong thing with what we meant to be used as text or JSON etc.
data ContentType
  = PlainText
  | JSON

renderContentType
  :: ContentType
  -> ByteString
renderContentType PlainText = "text/plain"
renderContentType JSON      = "application/json"

-----------------
-- Config Types
-----------------

-- This is an alternative way of defining a `newtype`. You define it as a simple
-- record and this lets you specify an unwrapping function at the same time. Which
-- technique you choose is a matter for your specific needs and preference.
--
newtype Port = Port
  { getPort :: Word16 }
  deriving (Eq, Show)

newtype DBFilePath = DBFilePath
  { getDBFilePath :: FilePath }
  deriving (Eq, Show)

-- The ``Conf`` type will need:
-- - A customisable port number: ``Port``
-- - A filepath for our SQLite database: ``DBFilePath``
data Conf = Conf
  { port       :: Port
  , dbFilePath :: DBFilePath
  }

-- We're storing our Port as a Word16 to be more precise and prevent invalid
-- values from being used in our application. However Wai is not so stringent.
-- To accommodate this and make our lives a bit easier, we will write this
-- helper function to take ``Conf`` value and convert it to an ``Int``.
confPortToWai
  :: Conf
  -> Int
confPortToWai =
  fromIntegral . getPort . port

-- Similar to when we were considering our application types, leave this empty
-- for now and add to it as you go.
data ConfigError
  = MissingPort
  | MissingDBFilePath
  deriving Show

-- Our application will be able to load configuration from both a file and
-- command line input. We want to be able to use the command line to temporarily
-- override the configuration from our file. How do we combine the different
-- inputs to enable this property?

-- We want the command line configuration to take precedence over the File
-- configuration, so if we think about combining each of our ``Conf`` records,
-- we want to be able to write something like this:

-- ``defaults <> file <> commandLine``

-- We can use the ``Monoid`` typeclass to handle combining the ``Conf`` records
-- together, and the ``Last`` type to wrap up our values to handle the desired
-- precedence. The ``Last`` type is a wrapper for Maybe that when used with its
-- ``Monoid`` instance will always preference the last ``Just`` value that it
-- has:

-- Last (Just 3) <> Last (Just 1) = Last (Just 1)
-- Last Nothing  <> Last (Just 1) = Last (Just 1)
-- Last (Just 1) <> Last Nothing  = Last (Just 1)

-- To make this easier, we'll make a new type ``PartialConf`` that will have our
-- ``Last`` wrapped values. We can then define a ``Monoid`` instance for it and
-- have our ``Conf`` be a known good configuration.
data PartialConf = PartialConf
  { pcPort       :: Last Port
  , pcDBFilePath :: Last DBFilePath
  }

-- We now define our ``Monoid`` instance for ``PartialConf``. Allowing us to
-- define our always empty configuration, which would always fail our
-- requirements. More interestingly, we define our ``mappend`` function to lean
-- on the ``Monoid`` instance for Last to always get the last value.
instance Monoid PartialConf where
  mempty = PartialConf mempty mempty

  mappend a b = PartialConf
    { pcPort       = pcPort a <> pcPort b
    , pcDBFilePath = pcDBFilePath a <> pcDBFilePath b
    }

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }
