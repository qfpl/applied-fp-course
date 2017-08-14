module FirstApp.Types.DB where

import           Data.Text                          (Text)
import           Data.Time                          (UTCTime)

import           Database.PostgreSQL.Simple.FromRow (FromRow (..), field)

data DbComment = DbComment
  { dbCommentId      :: Int
  , dbCommentTopic   :: Text
  , dbCommentComment :: Text
  , dbCommentTime    :: UTCTime
  }
  deriving Show

instance FromRow DbComment where
  fromRow = DbComment
            <$> field
            <*> field
            <*> field
            <*> field
