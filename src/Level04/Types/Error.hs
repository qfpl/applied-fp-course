{-# LANGUAGE OverloadedStrings #-}
module Level04.Types.Error
  ( Error(..)
  , nonEmptyText
  ) where

import Data.Text (Text)

data Error
  = UnknownRoute
  | EmptyCommentText
  | EmptyTopic
  -- Add another constructor for our DB error types.
  deriving (Eq, Show)

nonEmptyText
  :: (Text -> a)
  -> Error
  -> Text
  -> Either Error a
nonEmptyText _ e "" = Left e
nonEmptyText c _ tx = Right (c tx)
