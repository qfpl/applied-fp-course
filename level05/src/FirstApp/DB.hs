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

-- Because our `Table` is a configurable value, this application has a SQL
-- injection vulnerability. That being said, in order to leverage this weakness,
-- your appconfig.json file must be compromised and your app restarted. If that
-- is capable of happening courtesy of a hostile actor, there are larger issues.

-- Complete the withTable function so that the placeholder '$$tablename$$' is
-- found and replaced in the provided Query.
-- | withTable
-- >>> withTable (Table "tbl_nm") "SELECT * FROM $$tablename$$"
-- "SELECT * FROM tbl_nm"
-- >>> withTable (Table "tbl_nm") "SELECT * FROM foo"
-- "SELECT * FROM foo"
-- >>> withTable (Table "tbl_nm") ""
-- ""
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
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ = withTable tab
      "CREATE TABLE IF NOT EXISTS $$tablename$$ (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DbComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DbComment to a Comment, we need to use ``fromDbComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
getComments =
  -- There are several possible implementations of this function. Particularly
  -- there may be a trade-off between deciding to throw an Error if a DbComment
  -- cannot be converted to a Comment, or simply ignoring any DbComment that is
  -- not valid.
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
