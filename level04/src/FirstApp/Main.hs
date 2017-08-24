{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main (runApp, app) where

import           Network.Wai
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import qualified FirstApp.Conf            as Conf
import           FirstApp.Types

runApp :: IO ()
runApp = do
  cfgE <- Conf.parseOptions "appconfig.json"
  case cfgE of
    Left err  -> print err
    Right cfg -> run ( Conf.getPort $ Conf.port cfg ) ( app cfg )

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

app
  :: Conf.Conf
  -> Application
app cfg rq cb = mkRequest rq
  >>= fmap handleRespErr . pure . handleRErr
  >>= cb
  where
    -- Does this seem clunky to you?
    handleRespErr =
      either mkErrorResponse id
    -- Because it is clunky, and we have a better solution, later.
    handleRErr =
      either Left ( handleRequest cfg )

handleRequest
  :: Conf.Conf
  -> RqType
  -> Either Error Response
handleRequest cfg (AddRq _ _) =
  Right $ resp200 (Conf.mkMessage cfg)
handleRequest _ (ViewRq _) =
  Right $ resp200 "Susan was here"
handleRequest _ ListRq =
  Right $ resp200 "[ \"Fred was here\", \"Susan was here\" ]"

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

mkAddRequest
  :: Text
  -> LBS.ByteString
  -> Either Error RqType
mkAddRequest ti c = AddRq
  <$> mkTopic ti
  <*> (mkCommentText . decodeUtf8 $ LBS.toStrict c)

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
  resp404 "Unknown Route"
mkErrorResponse EmptyCommentText =
  resp400 "Empty Comment"
mkErrorResponse EmptyTopic =
  resp400 "Empty Topic"

