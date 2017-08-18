{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
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
                                                     Query (..), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.DB.Types
import           FirstApp.Error                     (Error (..))
import           FirstApp.Types

import           FirstApp.AppM                      (AppM, envDb, throwL)

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

-- If we're using AppM, then the Table information will be stored in our
-- ReaderT, so we can request it from there without requiring it to be passed.
-- If this location or type changes, then the compiler will complain so it won't
-- be a forgotten feature.
withTableM
  :: Query
  -> AppM Query
withTableM q =
  (`withTable` q) <$> asks (dbTable . envDb)

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
  -- or simply return the desired value.
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
