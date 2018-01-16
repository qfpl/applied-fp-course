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

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.Types                     (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDbComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

runDb
  :: (a -> Either Error b)
  -> IO a
  -> IO (Either Error b)
runDb f a = do
  r <- Sql.runDBAction a
  pure $ either (Left . DBError) f r
  -- Choices, choices...
  -- Sql.runDBAction a >>= pure . either (Left . DBError) f
  -- these two are pretty much the same.
  -- Sql.runDBAction >=> pure . either (Left . DBError) f
  -- this is because we noticed that our call to pure, which means we should
  -- be able to fmap to victory.
  -- fmap ( either (Left . DBError) f ) . Sql.runDBAction

getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
getComments db t = do
  -- Write the query with an icky string and remember your placeholders!
  let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  -- To be doubly and triply sure we've no garbage in our response, we take care
  -- to convert our DB storage type into something we're going to share with the
  -- outside world. Checking again for things like empty Topic or CommentText values.
  runDb ( traverse fromDbComment ) $ Sql.query (dbConn db) q [ getTopic t ]

addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
addCommentToTopic db t c = do
  -- Record the time this comment was created.
  nowish <- getCurrentTime
  -- Note the triple, matching the number of values we're trying to insert, plus
  -- one for the table name.
  let q =
        -- Remember that the '?' are order dependent so if you get your input
        -- parameters in the wrong order, the types won't save you here. More on that
        -- sort of goodness later.
        "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  -- We use the execute function this time as we don't care about anything
  -- that is returned. The execute function will still return the number of rows
  -- affected by the query, which in our case should always be 1.
  runDb Right $ Sql.execute (dbConn db) q (getTopic t, getCommentText c, nowish)
  -- An alternative is to write a returning query to get the Id of the DbComment
  -- we've created. We're being lazy (hah!) for now, so assume awesome and move on.

getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics db =
  let q = "SELECT DISTINCT topic FROM comments"
  in
    runDb (traverse ( mkTopic . Sql.fromOnly )) $ Sql.query_ (dbConn db) q

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic db t =
  let q = "DELETE FROM comments WHERE topic = ?"
  in
    runDb Right $ Sql.execute (dbConn db) q [getTopic t]
