module Level07.Types.CommentText
  ( CommentText
  , mkCommentText
  , getCommentText
  , encodeCommentText
  ) where

import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E

import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)

import           Level07.Types.Error        (Error (EmptyCommentText),
                                             nonEmptyText)

newtype CommentText = CommentText Text
  deriving (Show)

encodeCommentText :: Applicative f => Encoder f CommentText
encodeCommentText = getCommentText >$< E.text

mkCommentText
  :: Text
  -> Either Error CommentText
mkCommentText =
  nonEmptyText CommentText EmptyCommentText

getCommentText
  :: CommentText
  -> Text
getCommentText (CommentText t) =
  t
