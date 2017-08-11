module FirstApp.DB where

import Database.PostgreSQL.Simple (Connection)

import Data.Text (Text)

newtype Table = Table Text
  deriving Show

data FirstAppDB = FirstAppDB
  { dbConn :: Connection
  , dbTable :: Table
  }
