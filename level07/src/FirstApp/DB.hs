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

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Bifunctor                     (first)

import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.DB.Types                  (FirstAppDB (FirstAppDB, dbConn),
                                                     Table (Table, getTableName))
import           FirstApp.Error                     (Error (DBError))
import           FirstApp.Types                     (Comment, CommentText,
                                                     Topic, fromDbComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

import           FirstApp.AppM                      (AppM, envDb, throwL)

-- Doctest setup section
-- $setup
-- >>> :set -XOverloadedStrings

-- Quick helper to pull the connection and close it down.
closeDb
  :: FirstAppDB
  -> IO ()
closeDb =
  Sql.close . dbConn

-- Because our `Table` is as a configurable value, this application has a SQL
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
withTable t = Sql.Query
  -- This '$$foo$$' is our own made up placeholder value, it is not part of the Query type.
  . Text.replace "$$tablename$$" (getTableName t)
  . fromQuery

-- If we're using AppM, then the Table information will be stored in our
-- ReaderT, so we can request it from there without requiring it to be passed.
-- If this location or type changes, then the compiler will complain so it won't
-- be a forgotten feature.
withTableM
  :: Query
  -> AppM Query
withTableM =
  error "withTableM not implemented"

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
  :: ( Sql.Connection -> IO a )
  -> AppM a
runDb a = do
  db <- asks ( dbConn . envDb )
  -- We use the liftDb function to take the IO (Either SQLiteResponse a) and
  -- convert it into an `m (Either Error a)` so that it matches the requirements
  -- to be in our AppM, we then lean on the ExceptT functionality and use our
  -- helper to either `throwError` with any DB errors that have made it this far
  -- or return the desired value.
  liftDb db >>= throwL
  -- Or alternatively, if you hate variables...
  -- asks (dbConn.envDb) >>= ( liftDb >=> throwL )
  where
    -- The first function here is from the Data.Bifunctor module and lets us run
    -- functions on the left side of a Bifunctor:
    -- first :: ( a -> b ) -> p a c -> p b c
    -- Where `p` is our Bifunctor: Either
    liftDb conn = liftIO $ first DBError
      <$> Sql.runDBAction (a conn)

faQuery
  :: ( ToRow q
     , FromRow r
     )
  => Query
  -> q
  -> Connection
  -> IO [r]
faQuery q p c =
  Sql.query c q p

faQuery_
  :: FromRow r
  => Query
  -> Connection
  -> IO [r]
faQuery_ q c =
  Sql.query_ c q

faExecute
  :: ( ToRow q
     )
  => Query
  -> q
  -> Connection
  -> IO ()
faExecute q p c =
  Sql.execute c q p

getComments
  :: Topic
  -> AppM [Comment]
getComments t = do
  -- Write the query with an icky string and remember your placeholders!
  q <- withTableM "SELECT id,topic,comment,time FROM $$tablename$$ WHERE topic = ?"
  -- To be doubly and triply sure we've no garbage in our response, we take care
  -- to convert our DB storage type into something we're going to share with the
  -- outside world. Checking again for things like empty Topic or CommentText values.
  res <- runDb $ faQuery q [ getTopic t ]
  throwL $ traverse fromDbComment res

addCommentToTopic
  :: Topic
  -> CommentText
  -> AppM ()
addCommentToTopic t c = do
  -- Record the time this comment was created.
  nowish <- liftIO getCurrentTime
  -- Note the triple, matching the number of values we're trying to insert, plus
  -- one for the table name.
  q <- withTableM
        -- Remember that the '?' are order dependent so if you get your input
        -- parameters in the wrong order, the types won't save you here. More on that
        -- sort of goodness later.
        "INSERT INTO $$tablename$$ (topic,comment,time) VALUES (?,?,?)"
  -- We use the execute function this time as we don't care about anything
  -- that is returned. The execute function will still return the number of rows
  -- affected by the query, which in our case should always be 1.
  runDb $ faExecute q (getTopic t, getCommentText c, nowish)
  -- An alternative is to write a returning query to get the Id of the DbComment
  -- we've created. We're being lazy (hah!) for now, so assume awesome and move on.

getTopics
  :: AppM [Topic]
getTopics = do
  q <- withTableM "SELECT DISTINCT topic FROM $$tablename$$"
  res <- runDb $ faQuery_ q
  throwL $ traverse ( mkTopic . Sql.fromOnly ) res

deleteTopic
  :: Topic
  -> AppM ()
deleteTopic t = do
  q <- withTableM "DELETE FROM $$tablename$$ WHERE topic = ?"
  runDb $ faExecute q [getTopic t]
