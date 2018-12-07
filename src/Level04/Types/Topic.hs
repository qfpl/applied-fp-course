module Level04.Types.Topic
  ( Topic
  , mkTopic
  , getTopic
  , encodeTopic
  ) where

import           Waargonaut.Encode          (Encoder)
import qualified Waargonaut.Encode          as E

import           Data.Functor.Contravariant (contramap)
import           Data.Text                  (Text)

import           Level04.Types.Error        (Error (EmptyTopic), nonEmptyText)

newtype Topic = Topic Text
  deriving Show

mkTopic
  :: Text
  -> Either Error Topic
mkTopic =
  nonEmptyText Topic EmptyTopic

getTopic
  :: Topic
  -> Text
getTopic (Topic t) =
  t

-- | We will use this function to describe how we would like our `Topic`
-- type to be encoded into JSON.
--
-- Waargonaut knows how to encode a `Text` value, we need a way of telling it
-- how to unwrap our newtype to encode the `Text` value inside.
--
-- We _could_ write the code to unpack or pattern match on the `Topic` and
-- then run the `Text` encoder using that value as input before returning that
-- as the result of our Encoder. Something like this:
--
-- @
-- encodeA $ \(Topic t) -> runEncoder text t
-- @
--
-- But like many of the tasks that we've been completing in this course, the
-- plumbing for such a thing has already been written for us. Sometimes the
-- instances of the structure we're trying to create may provide a handy
-- shortcut.
--
-- In this case the `Encoder` type has an instance of `Contravariant`. Which has
-- the following function:
--
-- @
-- contramap :: Contravariant f => (a -> b) -> f b -> f a
-- @
--
-- In this case the `Encoder` type has an instance of `Contravariant`. That
-- typeclass has a function that comes in very handy when writing these
-- functions. There is a quick introduction to `Contravariant` in the `README`
-- for this level.
--
encodeTopic :: Applicative f => Encoder f Topic
encodeTopic = -- Try using 'contramap' and 'E.text'
  error "topic JSON encoder not implemented"
