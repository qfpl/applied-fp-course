{-# LANGUAGE OverloadedStrings #-}
module FirstApp.DB
  ( Table (..)
  , FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Bifunctor                     (first)

import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow,
                                                     Only (Only), fromOnly)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.DB.Types                  (FirstAppDB (FirstAppDB, dbConn),
                                                     Table (Table, getTableName))
import           FirstApp.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError), Topic,
                                                     fromDbComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           FirstApp.AppM                      (AppM, envDB, liftEither)

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
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: AppM Connection
getDBConn =
  asks (dbConn . envDB)

runDB
  :: (a -> Either Error b)
  -> (Connection -> IO a)
  -> AppM b
runDB f g =
  getDBConn >>= liftIO . g >>= liftEither . f

getComments
  :: Topic
  -> AppM [Comment]
getComments t =
  let
    q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
    a conn = Sql.query conn q [ getTopic t ]
  in
    runDB (traverse fromDbComment) a

addCommentToTopic
  :: Topic
  -> CommentText
  -> AppM ()
addCommentToTopic t c = do
  now <- liftIO getCurrentTime
  let
    q = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
    p = (getTopic t, getCommentText c, now)
    a conn = Sql.execute conn q p
  runDB pure a

getTopics
  :: AppM [Topic]
getTopics =
  let
    q = "SELECT DISTINCT(topic) FROM comments"
  in
    runDB (traverse (mkTopic . fromOnly)) $ \c -> Sql.query_ c q

deleteTopic
  :: Topic
  -> AppM ()
deleteTopic t =
  let
    q = "DELETE FROM comments WHERE topic = ?"
    p = Only . getTopic $ t
  in
    runDB pure $ \c -> Sql.execute c q p
