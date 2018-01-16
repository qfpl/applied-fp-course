{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module FirstApp.DB.Types where

import           Data.Text                      (Text)
import           Data.Time                      (UTCTime)

import           Database.SQLite.Simple.FromRow (FromRow (fromRow), field)

-- To try to avoid leaking various types and expected functionality around the
-- application, we create a stand alone type that will represent the data we
-- store in the database. In this instance, it is the raw types that make up a
-- comment.
--
-- Complete in the DbComment type below so it is a record type that matches the
-- Comment type, but without the newtype wrappers for each value.
data DBComment = DBComment
  deriving Show

-- This Typeclass comes from the `sqlite-simple` package and describes how to
-- decode a single row from the database into a single representation of our
-- type. This technique of translating a result row to a type will differ
-- between different packages/databases.
instance FromRow DBComment where

-- Now move to ``src/FirstApp/Types.hs``
