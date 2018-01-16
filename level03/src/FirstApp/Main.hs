{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main (runApp, app) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           FirstApp.Types           (ContentType (PlainText), Error (EmptyCommentText, EmptyTopic, UnknownRoute),
                                           RqType (AddRq, ListRq, ViewRq),
                                           mkCommentText, mkTopic,
                                           renderContentType)

runApp :: IO ()
runApp = run 3000 app

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse sts ct =
  responseLBS sts [(hContentType, renderContentType ct)]

resp200
  :: ContentType
  -> LBS.ByteString
  -> Response
resp200 =
  mkResponse status200

resp404
  :: ContentType
  -> LBS.ByteString
  -> Response
resp404 =
  mkResponse status404

resp400
  :: ContentType
  -> LBS.ByteString
  -> Response
resp400 =
  mkResponse status400
-- |

app :: Application
app rq cb = mkRequest rq
  >>= fmap handleRespErr . pure . handleRErr
  >>= cb
  where
    -- Does this seem clunky to you?
    handleRespErr :: Either Error Response -> Response
    handleRespErr = either mkErrorResponse id
    -- Because it is clunky, and we have a better solution, later.
    handleRErr :: Either Error RqType -> Either Error Response
    handleRErr = either Left handleRequest

handleRequest
  :: RqType
  -> Either Error Response
handleRequest (AddRq _ _) =
  Right $ resp200 PlainText "Hello there!"
handleRequest (ViewRq _) =
  Right $ resp200 PlainText "View Request not implemented"
handleRequest ListRq =
  Right $ resp200 PlainText "List Request not implemented"

mkRequest
  :: Request
  -> IO ( Either Error RqType )
mkRequest rq =
  case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) ->
      mkAddRequest t <$> strictRequestBody rq
    -- View the comments on a given topic
    ( [t, "view"], "GET" ) ->
      pure ( mkViewRequest t )
    -- List the current topics
    ( ["list"], "GET" )    ->
      pure mkListRequest
    -- Finally we don't care about any other requests so throw your hands in the air
    _                      ->
      pure ( Left UnknownRoute )

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 . LBS.toStrict) c

mkViewRequest
  :: Text
  -> Either Error RqType
mkViewRequest =
  fmap ViewRq . mkTopic

mkListRequest
  :: Either Error RqType
mkListRequest =
  Right ListRq

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownRoute =
  resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText =
  resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic =
  resp400 PlainText "Empty Topic"
