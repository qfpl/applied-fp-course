module FirstApp.Error
  ( Error (..)
  ) where

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  | DBError SQLiteResponse
  deriving ( Eq, Show )
