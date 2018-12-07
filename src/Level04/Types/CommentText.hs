module Level04.Types.CommentText
  ( CommentText
  , mkCommentText
  , getCommentText
  ) where

import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E

import           Level04.Types.Error        (Error (EmptyCommentText),
                                             nonEmptyText)

import           Data.Functor.Contravariant (contramap)
import           Data.Text                  (Text)

newtype CommentText = CommentText Text
  deriving Show

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

-- | We will use this function to describe how we would like our `CommentText`
-- type to be encoded into JSON.
--
-- Waargonaut knows how to encode a `Text` value, we need a way of telling it
-- how to unwrap our newtype to encode the `Text` value inside.
--
-- We _could_ write the code to unpack or pattern match on the `CommentText` and
-- then run the `Text` encoder using that value as input before returning that
-- as the result of our Encoder. Something like this:
--
-- @
-- encodeA $ \(CommentText t) -> runEncoder text t
-- @
--
-- But like many of the tasks that we've been completing in this course, the
-- plumbing for such a thing has already been written for us. Sometimes the
-- instances of the structure we're trying to create may provide a handy
-- shortcut.
--
-- In this case the `Encoder` type has an instance of `Contravariant`. That
-- typeclass has a function that comes in very handy when writing these
-- functions. There is a quick introduction to `Contravariant` in the `README`
-- for this level.
--
encodeCommentText :: Applicative f => Encoder f CommentText
encodeCommentText = -- Try using 'contramap' and 'E.text'.
  error "CommentText JSON encoder not implemented"
