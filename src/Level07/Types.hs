{-# LANGUAGE OverloadedStrings #-}
module Level07.Types
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
  , partialConfDecoder
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  , fromDBComment
  , confPortToWai
  , encodeComment
  , encodeTopic
  ) where

import           System.IO.Error                    (IOError)

import           GHC.Word                           (Word16)

import           Data.ByteString                    (ByteString)
import           Data.Text                          (pack)

import           Data.Functor.Contravariant         ((>$<))
import           Data.Monoid                        (Last (Last))
import           Data.Semigroup                     (Semigroup ((<>)))

import           Data.Time                          (UTCTime)
import qualified Data.Time.Format                   as TF

import           Waargonaut.Decode                  (CursorHistory, Decoder)
import qualified Waargonaut.Decode                  as D
import           Waargonaut.Decode.Error            (DecodeError)

import           Waargonaut.Encode                  (Encoder)
import qualified Waargonaut.Encode                  as E

import           Database.SQLite.Simple             (Connection)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Level07.DB.Types                   (DBComment (dbCommentComment, dbCommentId, dbCommentTime, dbCommentTopic))

import           Level07.Types.Error                (Error (DBError, EmptyCommentText, EmptyTopic, UnknownRoute))

import           Level07.Types.CommentText          (CommentText,
                                                     encodeCommentText,
                                                     getCommentText,
                                                     mkCommentText)

import           Level07.Types.Topic                (Topic, encodeTopic,
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

data RqType
  = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

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
  deriving Eq

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
  = BadConfFile (DecodeError, CursorHistory)
  | MissingPort
  | MissingDBFilePath
  | JSONDecodeError String
  | ConfigFileReadError IOError
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

-- Before we can define our ``Monoid`` instance for ``PartialConf``, we'll have
-- to define a Semigroup instance. We define our ``(<>)`` function to lean
-- on the ``Semigroup`` instance for Last to always get the last value.
instance Semigroup PartialConf where
  a <> b = PartialConf
    { pcPort       = pcPort a <> pcPort b
    , pcDBFilePath = pcDBFilePath a <> pcDBFilePath b
    }

-- We now define our ``Monoid`` instance for ``PartialConf``. Allowing us to
-- define our always empty configuration, which would always fail our
-- requirements. We just define `mappend` to be an alias of ``(<>)``
instance Monoid PartialConf where
  mempty = PartialConf mempty mempty
  mappend = (<>)

-- When it comes to reading the configuration options from the command-line, we
-- use the 'optparse-applicative' package. This part of the exercise has already
-- been completed for you, feel free to have a look through the 'CommandLine'
-- module and see how it works.
--
-- For reading the configuration from the file, we're going to use the Waargonaut
-- library to handle the parsing and decoding for us. In order to do this, we
-- have to tell waargonaut how to go about converting the JSON into our PartialConf
-- data structure.
partialConfDecoder :: Monad f => Decoder f PartialConf
partialConfDecoder = PartialConf
  <$> lastAt "port" D.integral Port
  <*> lastAt "dbFilePath" D.string DBFilePath
  where
    lastAt k d c = Last . fmap c <$> D.atKeyOptional k d

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }
