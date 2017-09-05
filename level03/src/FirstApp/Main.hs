{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module FirstApp.Main (runApp, app) where

import           Network.Wai
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)
import           Data.Monoid              ((<>))

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           FirstApp.Types

runApp :: IO ()
runApp = do
  -- Load up the configuration by providing a FilePath for the JSON config file.
  cfgE <- error "configuration not implemented"
  -- Loading the configuration can fail, so we have to take that into account now.
  case cfgE of
    Left err  -> undefined
    Right _cfg ->  run undefined undefined

-- | Some helper functions to make our lives a little more DRY.
mkResponse
  :: Status
  -> ContentType
  -> LBS.ByteString
  -> Response
mkResponse sts ct msg =
  responseLBS sts [(hContentType, renderContentType ct)] msg

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

-- Now that we have our configuration, pass it where it needs to go.
app
  :: a
  -> Application
app cfg rq cb =
  (handleRespErr . handleRErr <$> mkRequest rq) >>= cb
  where
    -- Does this seem clunky to you?
    handleRespErr =
      either mkErrorResponse id
    -- Because it is clunky, and we have a better solution, later.
    handleRErr =
      either Left ( handleRequest cfg )

-- Now we have some config, we can pull the helloMsg off it and use it in the
-- response.
handleRequest
  :: a
  -> RqType
  -> Either Error Response
handleRequest _cfg (AddRq _ _) =
  Right $ resp200 PlainText ("App says: " <> undefined)
handleRequest _ (ViewRq _) =
  Right $ resp200 PlainText "Susan was here"
handleRequest _ ListRq =
  Right $ resp200 PlainText "[ \"Fred was here\", \"Susan was here\" ]"

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
    -- Finally we don't care about any other requests so build an Error response
    _                      ->
      pure mkUnknownRouteErr

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

mkUnknownRouteErr
  :: Either Error RqType
mkUnknownRouteErr =
  Left UnknownRoute

mkErrorResponse
  :: Error
  -> Response
mkErrorResponse UnknownRoute =
  resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText =
  resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic =
  resp400 PlainText "Empty Topic"

