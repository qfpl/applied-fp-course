{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Types where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)

-- Working through the specification for our application, what are the
-- types of requests we're going to handle?

-- We have to be able to:
-- - Comment on a given topic
-- - View a topic and its comments
-- - List the current topics

-- To that end, we will create the following types:

-- AddRq : Which needs to the target topic, and the body of the comment.
-- ViewRq : Which needs the topic being requested.
-- ListRq : Which lists all of the current topics.
data RqType

-- Not everything goes according to plan, but it's important that our
-- types reflect when errors can be introduced into our program. Additionally
-- it's useful to be able to be descriptive about what went wrong.

-- Think about some of the basic things that can wrong with our Requests and
-- building the RqTypes, and create some values to represent that.
data Error
-- For now we don't need to worry about things like malformed requests or
-- invalid headers etc.

-- Provide a type to list our response content types so we don't try to
-- do the wrong thing with what we meant to be used as text/JSON etc.
data ContentType
--
-- The ContentType description for a header doesn't match our data definition
-- so we write a little helper function to pattern match on our ContentType
-- value and provide the correct header value.
renderContentType
  :: ContentType
  -> ByteString
renderContentType =
  error "renderContentType not implemented"

-- In Haskell the `newtype` comes with zero runtime cost. It is purely used for
-- typechecking. So when you have a bare 'primitive' value, like an Int, String, or
-- even [a], you can wrap it up in a `newtype` for clarity.

-- The type system will check it for you, and the compiler will eliminate the cost.

-- Having specialised constructor functions for the newtypes allows you to set
-- extra restrictions for your newtype.

-- Write two `newtype` definitions for `Topic` and `CommentText` that wrap a
-- `Text` value

-- Topic
--

-- Comment
--

-- |

-- A benefit of `newtype` is that we can choose to *not* export the constructor
-- and provide a function of our own. In our case, we're not interested in empty
-- `Text` values so we can eliminate them and immediately report an error.
mkTopic
  :: Text
  -> Either Error Topic
mkTopic =
  error "mkTopic not implemented"

mkCommentText
  :: Text
  -> Either Error CommentText
mkCommentText =
  error "mkCommentText not implemented"

-- After you've implemented these functions, adjust the export list in the
-- module declaration so your constructor functions cannot be bypassed. Also
-- export the functions that let you access the inner value of the newtype.

