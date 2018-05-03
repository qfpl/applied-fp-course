{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
module Level02.Types
  ( Topic
  , CommentText
  , ContentType (..)
  , RqType (..)
  , Error (..)
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  ) where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)

-- Working through the specification for our application, what are the
-- types of requests we're going to handle?

-- We have to be able to:
-- - Comment on a given topic
-- - View a topic and its comments
-- - List the current topics

-- The first thing we can do is provide some safety and structure around our
-- ``Topic`` and ``CommentText`` types. Since we don't want to deal with bare
-- ``Text`` values, we'll define some ``newtype`` wrappers for these types to
-- make our code easier to work with.

-- In Haskell the ``newtype`` is a wrapper of sorts that comes with zero runtime
-- cost. It is purely used for type-checking as it constructs an entirely new
-- type, not just a type alias. So when you have a bare primitive value, like an
-- ``Int``, ``String``, or even ``[a]``, you can wrap it up in a ``newtype`` to
-- give it a descriptive name to be more precise in your types.

-- The type system will check it for you, and the compiler will eliminate the
-- cost of the "wrapper". Also, having specialised constructor functions for the
-- newtypes allows you to set extra restrictions, such as minimum values.

-- The ``newtype`` definitions for ``Topic`` and ``CommentText`` are provided
-- for you:

-- Topic
newtype Topic = Topic Text
  deriving Show

-- CommentText
newtype CommentText = CommentText Text
  deriving Show

-- Using these convenient definitions, we can create the following constructors
-- for our RqType:
--
-- AddRq : Which needs the target topic, and the body of the comment.
-- ViewRq : Which needs the topic being requested.
-- ListRq : Which doesn't need anything and lists all of the current topics.
data RqType

-- Not everything goes according to plan, but it's important that our types
-- reflect when errors can be introduced into our program. Additionally it's
-- useful to be able to be descriptive about what went wrong.

-- Fill in the error constructors as you need them.
data Error


-- Provide the constructors for a sum type to specify the `ContentType` Header,
-- to be used when we build our Response type. Our application will be simple,
-- we will only have a need for two content types:
--
-- - plain text
-- - json
data ContentType

-- The ``ContentType`` constructors don't match what is required for the header
-- information. Because ``wai`` uses a stringly type. So write a function that
-- will take our ``ContentType`` and produce the correct value for the header.
--
-- We have included the return types for the different content types below,
-- write this function such that your ContentType constructors match the
-- following values:
--
-- - plain text = "text/plain"
-- - json       = "application/json"
--
renderContentType
  :: ContentType
  -> ByteString
renderContentType =
  error "renderContentType not implemented"

-- We can choose to *not* export the constructor for a data type and instead
-- provide a function of our own. In our case, we're not interested in empty
-- `Text` values so we will eliminate them with a special constructor and return
-- an error if an empty input is provided.
--
-- The export list at the top of this file demonstrates how to export a type,
-- but not export the constructor.

mkTopic
  :: Text
  -> Either Error Topic
mkTopic =
  error "mkTopic not implemented"

getTopic
  :: Topic
  -> Text
getTopic =
  error "getTopic not implemented"

mkCommentText
  :: Text
  -> Either Error CommentText
mkCommentText =
  error "mkCommentText not implemented"

getCommentText
  :: CommentText
  -> Text
getCommentText =
  error "getCommentText not implemented"

---- Go to `src/Level02/Core.hs` next
