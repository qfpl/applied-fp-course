{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main (runApp) where

import Network.Wai
import Network.Wai.Handler.Warp (run)

import Network.HTTP.Types (Status, status200, status404, status400, hContentType)

import qualified Data.ByteString.Lazy as LBS

import Data.Either (either)

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import FirstApp.Types

runApp :: IO ()
runApp = run 3000 app

-- | Just some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse sts ct msg =
  responseLBS sts [(hContentType, renderContentType ct)] msg

resp200
  :: LBS.ByteString
  -> Response
resp200 =
  mkResponse status200 PlainText

resp404
  :: LBS.ByteString
  -> Response
resp404 =
  mkResponse status404 PlainText

resp400
  :: LBS.ByteString
  -> Response
resp400 =
  mkResponse status400 PlainText
-- |

{-|
How can we use the types to make this better?

We need a way to go from the pathInfo and requestMethod to a Request type
that matches our current specification.
-}
app
  :: Application
app rq cb = mkRequest rq
  >>= fmap handleRespErr . handleRErr
  >>= cb
  where
    -- Does this seem clunky to you?
    handleRespErr =
      either mkErrorResponse id
    -- Because it is clunky, and we have a better solution, later.
    handleRErr =
      either ( pure . Left ) handleRequest

{-|
Lets use our RqTypes to write a function that will take the input from the
Wai library and turn it into something our application cares about.
-}
mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest rq =
  case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) -> mkAddRequest t <$> strictRequestBody rq
    -- View the comments on a given topic
    ( [t, "view"], "GET" ) -> pure ( mkViewRequest t )
    -- List the current topics
    ( ["list"], "GET" )    -> pure mkListRequest
    -- Finally we don't care about any other requests so throw your hands in the air
    _                      -> pure mkUnknownRouteErr

-- These helpers will take the raw request information and turn it into
-- one of our data types. This means we draw a line about where the unruly outside
-- world must end, and where the well-typed world of our application begins.
mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 $ LBS.toStrict c)

-- This has other benefits, we're able isolate our validation requirements into the
-- smallest chunks we can manage. This allows for fantastic reuse and it also means
-- that validation is not spread across the application. It is kept at the borders.
mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest =
  fmap ViewRq . mkTopic

-- Even thought it may seem trivial or even pointless to write functions such as these
-- it allows for much greater consistency across the application.

-- These are straight forward data constructors, but by doing it this way we don't
-- have any snowflakes littered about the code. It also enhances our ability to
-- spot larger patterns in our application, which are opportunities for abstraction.
mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

{-|
HALP

Alternative type sig:
Either Error a

But iirc this isn't as protected against being used in the wrong spot, since the `a`
is polymorphic we could mess up and use this where we're trying to return a Topic.
-}
mkUnknownRouteErr
  :: Either Error RqType
mkUnknownRouteErr =
  Left UnknownRoute

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownRoute =
  resp404 "Unknown Route"
mkErrorResponse EmptyCommentText =
  resp400 "Empty Comment"
mkErrorResponse EmptyTopic =
  resp400 "Empty Topic"
-- mkErrorResponse _ =
--   error "mkErrorResponse not implemented"

{-|
We'll stub these for now as the general structure and the process of reaching
this stage is the more important lesson here.

Notice how we're only accepting our predefined request types that have the required
information already validated and prepared for use in the handling of the request.

If we find that we need more information to handle a request, or we have a new
type of request that we'd like to handle then we simply update the RqType structure
and the compiler will let us know the affected portions of our application.

Reduction of concerns such that each section of the application only deals with
a small piece is one of the benefits of developing in this way.
-}
handleRequest
  :: RqType
  -> IO (Either Error Response)
handleRequest (AddRq _ _) =
  pure . Right $ resp200 "Fred wuz ere"
handleRequest (ViewRq _) =
  pure . Right $ resp200 "Susan was ere"
handleRequest ListRq =
  pure . Right $ resp200 "Fred wuz ere, Susan was ere"
-- handleRequest _ =
--   error "handleRequest not implemented"
