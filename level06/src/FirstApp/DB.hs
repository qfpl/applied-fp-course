{-# LANGUAGE OverloadedStrings #-}
module FirstApp.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)

import Data.Bifunctor (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow, ToRow,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import FirstApp.AppM (AppM, Env (envDB))

import           FirstApp.Types                     (FirstAppDB (FirstAppDB, dbConn), Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError), Topic,
                                                     fromDbComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: AppM Connection
getDBConn =
  error "getDBConn not implemented"

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> AppM (Either Error b)
runDB =
  error "runDB not re-implemented"

getComments
  :: Topic
  -> AppM (Either Error [Comment])
getComments =
  error "Copy your completed 'getComments' and refactor to match the new type signature"

addCommentToTopic
  :: Topic
  -> CommentText
  -> AppM (Either Error ())
addCommentToTopic =
  error "Copy your completed 'appCommentToTopic' and refactor to match the new type signature"

getTopics
  :: AppM (Either Error [Topic])
getTopics =
  error "Copy your completed 'getTopics' and refactor to match the new type signature"

deleteTopic
  :: Topic
  -> AppM (Either Error ())
deleteTopic =
  error "Copy your completed 'deleteTopic' and refactor to match the new type signature"
