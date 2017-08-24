{-# LANGUAGE OverloadedStrings #-}
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

import           Database.SQLite.Simple             (Connection, Query (..))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.Types

newtype Table = Table
  { getTableName :: Text }
  deriving Show

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
data FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  , dbTable :: Table
  }

-- Quick helper to pull the connection and close it down.
closeDb
  :: FirstAppDB
  -> IO ()
closeDb =
  Sql.close . dbConn

-- Due to the way our application is designed, we have a slight SQL injection
-- risk because we pull the table name from the config or input arguments. This
-- attempts to mitigate that somewhat by removing the need for repetitive string
-- mangling when building our queries. We simply write the query and pass it
-- through this function that requires the Table information and everything is
-- taken care of for us. This is probably not the way to do things in a large
-- scale app.
withTable
  :: Table
  -> Query
  -> Query
withTable t = Sql.Query
  . Text.replace "$$tablename$$" (getTableName t)
  . fromQuery

initDb
  :: FilePath
  -> Table
  -> IO ( Either SQLiteResponse FirstAppDB )
initDb fp tab = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open fp
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con tab
  where
  -- Query has a IsString instance so you can write straight strings like this
  -- and it will convert them into a Query type, use '?' as placeholders for
  -- ORDER DEPENDENT interpolation.
    createTableQ = withTable tab
      "CREATE TABLE IF NOT EXISTS $$tablename$$ (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

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
  -- just be able to fmap to victory.
  -- fmap ( either (Left . DBError) f ) . Sql.runDBAction

getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
getComments db t = do
  -- Write the query with an icky string and remember your placeholders!
  let q = withTable (dbTable db)
        "SELECT id,topic,comment,time FROM $$tablename$$ WHERE topic = ?"
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
  let q = withTable (dbTable db)
        -- Remember that the '?' are order dependent so if you get your input
        -- parameters in the wrong order, the types won't save you here. More on that
        -- sort of goodness later.
        "INSERT INTO $$tablename$$ (topic,comment,time) VALUES (?,?,?)"
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
  let q = withTable (dbTable db) "SELECT DISTINCT topic FROM $$tablename$$"
  in
    runDb (traverse ( mkTopic . Sql.fromOnly )) $ Sql.query_ (dbConn db) q

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic db t =
  let q = withTable (dbTable db) "DELETE FROM $$tablename$$ WHERE topic = ?"
  in
    runDb Right $ Sql.execute (dbConn db) q [getTopic t]
