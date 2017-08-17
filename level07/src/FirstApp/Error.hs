module FirstApp.Error
  ( Error (..)
  ) where

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

{-|
Not everything goes according to plan, but it's important that our
types reflect when errors can be introduced into our program. Additionally
it's useful to be able to be descriptive about what went wrong.

So lets think about some of the basic things that can wrong with our
program and create some values to represent that.
-}
data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  -- This is our new error constructor.
  | DBError SQLiteResponse
  deriving Show
