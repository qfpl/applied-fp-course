{-# LANGUAGE OverloadedStrings #-}
-- This is an example module if you wanted to use PostgreSQL with the
-- postgresql-simple package. It is missing the very helpful error handling of
-- the sqlite-simple-errors package. The postgresql-simple package is littered
-- with exceptions which are not visible in the types, so be wary.
module FirstApp.DB.PostgreSQL where

-- import           GHC.Int                          (Int64)

-- import           Data.Text                        (Text)
-- import           Data.Time                        (getCurrentTime)

-- import           Database.PostgreSQL.Simple       (Connection, FromRow, Query,
--                                                    ToRow)
-- import           Database.PostgreSQL.Simple.Types (Identifier (..))
-- import qualified Database.PostgreSQL.Simple       as PG

-- import           FirstApp.Types

-- newtype Table = Table
--   { getTableName :: Text }
--   deriving Show

-- -- This is a bit more configuration available to the PostgreSQL package so we
-- -- have a bit more work to do.
-- newtype DbName = DbName
--   { getDbName :: String }
--   deriving Show

-- newtype UserName = UserName
--   { getUserName :: String }
--   deriving Show

-- data FirstAppDB = FirstAppDB
--   { dbConn  :: Connection
--   }

-- closeDb
--   :: FirstAppDB
--   -> IO ()
-- closeDb =
--   PG.close . dbConn

-- initDb
--   :: UserName
--   -> DbName
--   -> IO FirstAppDB
-- initDb un dbN tab = do
--   -- The ConnectInfo type from PostgreSQL has extra configuration options if your local setup is a bit different
--   -- https://hackage.haskell.org/package/postgresql-simple-0.5.3.0/docs/Database-PostgreSQL-Simple.html#v:defaultConnectInfo
--   --
--   -- Use the info to adjust the default connection options.
--   let info = PG.defaultConnectInfo
--              { PG.connectUser     = getUserName un
--              , PG.connectDatabase = getDbName dbN
--              }
--   -- Initialise the connection to the DB...
--   -- - What could go wrong here?
--   -- - What haven't we been told in the types?
--   con <- PG.connect info
--   -- Initialise our one table, if it's not there already
--   _ <- PG.execute con createTableQ
--   -- Wrap it up and hand it back.
--   pure $ FirstAppDB con tab

-- createTableQ
--   :: PG.Query
-- createTableQ =
--   -- Query has a IsString instance so you can write straight strings like this
--   -- and it will convert them into a Query type, use '?' as placeholders for
--   -- ORDER DEPENDENT interpolation.
--   "CREATE TABLE IF NOT EXISTS ? (id SERIAL PRIMARY KEY, topic TEXT, comment TEXT, time TIMESTAMPTZ)"
--   -- Another way to express this query if you prefer being able to use line
--   -- breaks is to use the QuasiQuotes extension and write the following:
--   -- [sql| CREATE TABLE IF NOT EXISTS comments (
--   --       id SERIAL PRIMARY KEY,
--   --       topic TEXT,
--   --       comment TEXT,
--   --       time TIMESTAMPTZ
--   --     )
--   --     |]

-- getComments
--   :: FirstAppDB
--   -> Topic
--   -> IO (Either Error [Comment])
-- getComments db t = do
--   -- Write the query with an icky string and remember your placeholders!
--   let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
--   -- Run the query against our DB using our connection.
--   -- To build the replacements for the query placeholders, this package uses
--   -- tuples. Remember that the '?' are order dependent so if you get your input
--   -- parameters in the wrong order, the types won't save you here. More on that
--   -- sort of goodness later.
--   res <- PG.query (dbConn db) q (Only $ getTopic t)
--   -- To be doubly and triply sure we've no garbage in our response, we take care
--   -- to convert our DB storage type into something we're going to share with the
--   -- outside world. Checking again for things like empty Topic or CommentText
--   -- values.
--   pure $ traverse fromDbComment res
--   -- Note that because of the use of traverse, this function will fail at the
--   -- first record that is invalid and discard any successful values.

-- addCommentToTopic
--   :: FirstAppDB
--   -> Topic
--   -> CommentText
--   -> IO (Either Error ())
-- addCommentToTopic db t c = do
--   -- Record the time this comment was created.
--   nowish <- getCurrentTime
--   -- Note the triple, matching the number of values we're trying to insert, plus
--   -- one for the table name.
--   let q = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
--   -- We use the PG.execute function this time as we don't care about anything
--   -- that is returned. The execute function will still return the number of rows
--   -- affected by the query, which in our case should always be 1.
--   res <- PG.execute (dbConn db) q (getTopic t, getCommentText c, nowish)
--   -- An alternative is to write a returning query to get the Id of the DbComment
--   -- we've created. We're being pretty lazy right now so check we've
--   -- affected a single row and move on.
--   pure $ if res == 1 then Right ()
--     else Left (DBError "Comment Insert Failed")

-- getTopics
--   :: FirstAppDB
--   -> IO (Either Error [Topic])
-- getTopics db = do
--   let q = "SELECT DISTINCT topic FROM comments"
--   res <- PG.query_ (dbConn db) q
--   pure $ traverse ( mkTopic . PG.fromOnly ) res
