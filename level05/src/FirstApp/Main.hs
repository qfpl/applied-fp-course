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

import Data.Aeson (ToJSON)
import qualified Data.Aeson as A

import qualified FirstApp.DB as DB
import qualified FirstApp.Conf as Conf
import           FirstApp.Types

runApp :: IO ()
runApp = do
  cfgE <- Conf.parseOptions "appconfig.json"
  case cfgE of
    Left err  -> print err
    Right cfg -> do

      db <- DB.initDb
        (DB.UserName "manky")
        (Conf.dbName cfg)
        (Conf.tableName cfg)

      run ( Conf.getPort $ Conf.port cfg ) ( app cfg db )

      DB.closeDb db

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

resp200Json
  :: ToJSON a
  => a
  -> Response
resp200Json =
  mkResponse status200 JSON . A.encode

-- |

app
  :: Conf.Conf
  -> DB.FirstAppDB -- ^ Add the Database record to our app so we can use it
  -> Application
app cfg db rq cb = mkRequest rq
  >>= fmap handleRespErr . handleRErr
  >>= cb
  where
    -- Does this seem clunky to you?
    handleRespErr =
      either mkErrorResponse id
    -- Because it is clunky, and we have a better solution, later.
    handleRErr =
      -- We want to pass the Database through to the handleRequest so it's
      -- available to all of our handlers.
      either ( pure . Left ) ( handleRequest cfg db )

handleRequest
  :: Conf.Conf
  -> DB.FirstAppDB
  -> RqType
  -> IO (Either Error Response)
-- Fun time to play with some type driven development. Try inserting a
-- type-hole on the left of the getComments function call and see what sort of
-- functions you need to produce the desired output. See how well you can
-- reduce the function you need to write with the application of abstractions
-- you already know, no custom functions.
handleRequest _ db (AddRq t c) =
  fmap (\_ -> resp200 "Success") <$> DB.addCommentToTopic db t c
handleRequest _ db (ViewRq t) =
  fmap resp200Json <$> DB.getComments db t
handleRequest _ db ListRq =
  fmap resp200Json <$> DB.getTopics db

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

