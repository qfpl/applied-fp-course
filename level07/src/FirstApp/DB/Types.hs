module FirstApp.DB.Types where

import           Data.Time                      (UTCTime)

import           Data.Text                      (Text)

import           Database.SQLite.Simple         (Connection)
import           Database.SQLite.Simple.FromRow (FromRow (fromRow), field)

newtype Table = Table
  { getTableName :: Text }
  deriving Show

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
data FirstAppDB = FirstAppDB
  { dbConn  :: Connection
  }

-- To try to avoid leaking various types and expected functionality around the
-- application, we create a stand alone type that will represent the data we
-- store in the database. In this instance, it is the raw types that make up a
-- comment.
data DbComment = DbComment
  { dbCommentId      :: Int
  , dbCommentTopic   :: Text
  , dbCommentComment :: Text
  , dbCommentTime    :: UTCTime
  }
  deriving Show

-- This type class instance comes from our DB package and tells the DB package
-- how to decode a single row from the database into a single representation of
-- our type. This technique of translating a result row to a type will differ
-- between different packages/databases.
instance FromRow DbComment where
  fromRow = DbComment
            -- field :: FromField a => RowParser a
            <$> field
            <*> field
            <*> field
            <*> field
