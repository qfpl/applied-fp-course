{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module FirstApp.DB
  ( Table (..)
  , FirstAppDB (FirstAppDB)
  , initDb
  , closeDb
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, Query (Query))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.Types                     (Comment, CommentText,
                                                     Error, Topic)

-- ------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple ready for this section! |
-- ------------------------------------------------------------------------|

-- We need to have a way to pass around the name of the Table we're going to us
-- for the comments in this application. We _could_ pass around a `Text` value,
-- but we can be better than that.
newtype Table = Table Text
  deriving Show

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
data FirstAppDB = FirstAppDB

-- Quick helper to pull the connection and close it down.
closeDb
  :: FirstAppDB
  -> IO ()
closeDb =
  error "closeDb not implemented"

-- Because our `Table` is as a configurable value, this application has a SQL
-- injection vulnerability. Write a function that attempts to mitigate that
-- risk, by handling replacement of a place-holder value in a given `Query`. We
-- should be able to write the query and pass it through this function and
-- everything is will be taken care of for us.

-- This is _not_ the way to do things in a large scale app, obviously.
withTable
  :: Table
  -> Query
  -> Query
withTable =
  error "withTable not yet implemented"

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDb
  :: FilePath
  -> Table
  -> IO ( Either SQLiteResponse FirstAppDB )
initDb fp tab =
  error "initDb not implemented"
  where
  -- Query has a `IsString` instance so you can write straight strings like this
  -- and it will convert them into a `Query` type, use '?' as place-holders for
  -- ORDER DEPENDENT interpolation.
    createTableQ = withTable tab
      "CREATE TABLE IF NOT EXISTS $$tablename$$ (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DbComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
getComments =
  error "getComments not implemented"

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
addCommentToTopic =
  error "addCommentToTopic not implemented"

getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics =
  error "getTopics not implemented"

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic =
  error "deleteTopic not implemented"
