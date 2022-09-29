{-# LANGUAGE OverloadedStrings #-}

module Level04.Types.Error (
  Error (..),
  nonEmptyText,
) where

import Data.Text (Text)
import qualified Database.SQLite.SimpleErrors.Types as Sql

data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  | SqlError Sql.SQLiteResponse
  deriving (Eq, Show)

nonEmptyText ::
  (Text -> a) ->
  Error ->
  Text ->
  Either Error a
nonEmptyText _ e "" = Left e
nonEmptyText c _ tx = Right (c tx)
