{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module FirstApp.DB.Types where

import           Data.Text                      (Text)
import           Data.Time                      (UTCTime)

import           Database.SQLite.Simple.FromRow (FromRow (..), field)

-- To try to avoid leaking various types and expected functionality around the
-- application, we create a stand alone type that will represent the data we
-- store in the database. In this instance, it is the raw types that make up a
-- comment.
data DbComment = DbComment
  deriving Show

-- This Typeclass comes from the `sqlite-simple` package and describes how to
-- decode a single row from the database into a single representation of our
-- type. This technique of translating a result row to a type will differ
-- between different packages/databases.
instance FromRow DbComment where
