{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main (runApp) where

import           Network.Wai
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           FirstApp.Types


-- ---------------------------------------------|
--  Don't start here, go to FirstApp.Types! :)  |
-- ---------------------------------------------|

runApp :: IO ()
runApp = run 3000 app

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse =
  error "mkResponse not implemented"

resp200
  :: LBS.ByteString
  -> Response
resp200 =
  error "resp200 not implemented"

resp404
  :: LBS.ByteString
  -> Response
resp404 =
  error "resp404 not implemented"

resp400
  :: LBS.ByteString
  -> Response
resp400 =
  error "resp400 not implemented"
-- |

{-|
Lets use our RqTypes to write a function that will take the input from the
Wai library and turn it into something our application cares about.
-}
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest rq =
  error "mkRequest not implemented"

-- These helpers will take the raw request information and turn it into
-- one of our data types. This means we draw a line about where the unruly outside
-- world must end, and where the well-typed world of our application begins.

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest ti c =
  error "mkAddRequest not implemented"

-- This has other benefits, we're able isolate our validation requirements into the
-- smallest chunks we can manage. This allows for fantastic reuse and it also means
-- that validation is not spread across the application. It is kept at the borders.

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest =
  error "mkViewRequest not implemented"

-- Even though it may seem too trivial or even pointless to write functions such
-- as these it allows for much greater consistency across the application.
--
-- Some of these are straight forward data constructors, but by doing it this
-- way we don't have any snowflakes littered about the code. It also enhances
-- our ability to spot larger patterns in our application, which are
-- opportunities for abstraction.
mkListRequest
  :: Either Error RqType
mkListRequest =
  error "mkListRequest not implemented"

{-|
HALP - wording - is it useful?

Alternative type sig:
Either Error a

But iirc this isn't as protected against being used in the wrong spot, since the `a`
is polymorphic we could mess up and use this where we're trying to return a Topic.
-}
mkUnknownRouteErr
  :: Either Error RqType
mkUnknownRouteErr =
  error "mkUnknownRouteErr not implemented"

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse _ =
  error "mkErrorResponse not implemented"

-- Notice how we're only accepting our predefined request types that have the
-- required information already validated and prepared for use in the handling
-- of the request.
--
-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the RqType
-- structure and the compiler will let us know the affected portions of our
-- application.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
handleRequest
  :: RqType
  -> Either Error Response
handleRequest _ =
  error "handleRequest not implemented"

-- Reimplement our `app` function using the new functions and the RqTypes as a
-- guide.
app
  :: Application
app rq cb =
  error "app not reimplemented"
