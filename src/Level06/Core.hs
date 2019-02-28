{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Level06.Core
  ( runApplication
  , app
  , prepareAppReqs
  ) where

import qualified Control.Exception                  as Ex
import           Control.Monad.IO.Class             (liftIO)

import           Control.Monad.Except               (catchError, throwError)

import           Network.Wai                        (Application, Request,
                                                     Response, pathInfo,
                                                     requestMethod, responseLBS,
                                                     strictRequestBody)
import           Network.Wai.Handler.Warp           (run)

import           Network.HTTP.Types                 (Status, hContentType,
                                                     status200, status400,
                                                     status404, status500)

import qualified Data.ByteString.Lazy               as LBS

import           Data.Bifunctor                     (first)
import           Data.Either                        (either)
import           Data.Monoid                        ((<>))

import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)
import           Data.Text.Lazy.Encoding            (encodeUtf8)

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Waargonaut.Encode                  (Encoder')
import qualified Waargonaut.Encode                  as E

import           Level06.AppM                       (App, AppM (..),
                                                     liftEither, runApp)
import qualified Level06.Conf                       as Conf
import qualified Level06.DB                         as DB
import           Level06.Types                      (Conf, ConfigError,
                                                     ContentType (..),
                                                     Error (..),
                                                     RqType (AddRq, ListRq, ViewRq),
                                                     encodeComment, encodeTopic,
                                                     mkCommentText, mkTopic,
                                                     renderContentType)

-- | Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = DBInitErr SQLiteResponse
  | ConfErr ConfigError
  deriving Show

runApplication :: IO ()
runApplication = error "copy your previous 'runApp' implementation and refactor as needed"

-- | We need to complete the following steps to prepare our app requirements:
--
-- 1) Load the configuration.
-- 2) Attempt to initialise the database.
-- 3) Combine the results into a tuple
--
-- The file path for our application config is: "files/appconfig.json"
--
-- The config loading process is starting to become unweildly. We will re-use
-- our generalised AppM to also remove the problem of handling errors on start
-- up!
--
prepareAppReqs :: AppM StartUpError (Conf, DB.FirstAppDB)
prepareAppReqs = error "copy your prepareAppReqs from the previous level."

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

resp500
  :: ContentType
  -> LBS.ByteString
  -> Response
resp500 =
  mkResponse status500

resp200Json
  :: Encoder' a
  -> a
  -> Response
resp200Json e =
  resp200 JSON . encodeUtf8 .
  E.simplePureEncodeTextNoSpaces e

-- | Now that we have our configuration, pass it where it needs to go.
app
  :: Conf
  -> DB.FirstAppDB
  -> Application
app cfg db rq cb =
  runApp (handleRequest db =<< mkRequest rq) >>= cb . handleRespErr
  where
    handleRespErr :: Either Error Response -> Response
    handleRespErr = either mkErrorResponse id

handleRequest
  :: DB.FirstAppDB
  -> RqType
  -> App Response
handleRequest db rqType =
  case rqType of
    AddRq t c -> resp200 PlainText "Success"        <$  DB.addCommentToTopic db t c
    ViewRq t  -> resp200Json (E.list encodeComment) <$> DB.getComments db t
    ListRq    -> resp200Json (E.list encodeTopic)   <$> DB.getTopics db

mkRequest
  :: Request
  -> App RqType
mkRequest rq =
  liftEither =<< case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) -> liftIO $ mkAddRequest t <$> strictRequestBody rq
    -- View the comments on a given topic
    ( [t, "view"], "GET" ) -> pure ( mkViewRequest t )
    -- List the current topics
    ( ["list"], "GET" )    -> pure mkListRequest
    -- Finally we don't care about any other requests so build an Error response
    _                      -> pure ( Left UnknownRoute )

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
mkErrorResponse ( DBError _ ) =
  -- Be a sensible developer and don't leak your DB errors over the internet.
  resp500 PlainText "Oh noes"
