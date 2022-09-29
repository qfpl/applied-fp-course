{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Level04.Types.Topic (
  Topic,
  mkTopic,
  getTopic,
) where

import Data.Functor.Contravariant (contramap)
import Data.Text (Text)

import Data.Aeson (ToJSON)
import Database.SQLite.Simple (FromRow (fromRow), ToRow (toRow))
import Database.SQLite.Simple.FromField (FromField)
import Database.SQLite.Simple.ToField
import GHC.Generics (Generic)
import Level04.Types.Error (Error (EmptyTopic), nonEmptyText)

newtype Topic = Topic Text
  deriving (Show, Generic)

mkTopic ::
  Text ->
  Either Error Topic
mkTopic =
  nonEmptyText Topic EmptyTopic

getTopic ::
  Topic ->
  Text
getTopic (Topic t) =
  t

instance ToRow Topic -- ??

instance ToField Topic where
  toField (Topic t) = toField t

-- Vi vil encode Topic som helt vanlig tekst
-- Se : https://hackage.haskell.org/package/aeson-2.1.1.0/docs/Data-Aeson.html
--

instance ToJSON Topic

{- | We will use this function to describe how we would like our `Topic`
 type to be encoded into.

 Waargonaut knows how to encode a `Text` value, we need a way of telling it
 how to unwrap our newtype to encode the `Text` value inside.

 We _could_ write the code to unpack or pattern match on the `Topic` and
 then run the `Text` encoder using that value as input before returning that
 as the result of our Encoder. Something like this:

 @
 encodeA $ \(Topic t) -> runEncoder text t
 @

 But like many of the tasks that we've been completing in this course, the
 plumbing for such a thing has already been written for us. Sometimes the
 instances of the structure we're trying to create may provide a handy
 shortcut.

 In this case the `Encoder` type has an instance of `Contravariant`. Which has
 the following function:

 @
 contramap :: Contravariant f => (a -> b) -> f b -> f a
 @

 In this case the `Encoder` type has an instance of `Contravariant`. That
 typeclass has a function that comes in very handy when writing these
 functions. There is a quick introduction to `Contravariant` in the `README`
 for this level.

 encodeTopic :: Applicative f => Encoder f Topic
 encodeTopic = -- Try using 'contramap' and 'E.text'
   error "topic encoder not implemented"
-}
