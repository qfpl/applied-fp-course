{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Level06.Core
  ( runApp
  , app
  , prepareAppReqs
  ) where

import           Control.Monad.IO.Class             (liftIO)

import           Network.Wai                        (Application, Request,
                                                     Response, pathInfo,
                                                     requestMethod, responseLBS,
                                                     strictRequestBody)
import           Network.Wai.Handler.Warp           (run)

import           Network.HTTP.Types                 (Status, hContentType,
                                                     status200, status400,
                                                     status404, status500)

import qualified Data.ByteString.Lazy               as LBS

import           Data.Either                        (either)
import           Data.Monoid                        ((<>))

import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           Data.Aeson                         (ToJSON)
import qualified Data.Aeson                         as A

import           Level06.AppM                       (AppM, liftEither, runAppM)
import qualified Level06.Conf                       as Conf
import qualified Level06.DB                         as DB
import           Level06.Types                      (Conf, ContentType (..),
                                                     Error (..),
                                                     RqType (AddRq, ListRq, ViewRq),
                                                     mkCommentText, mkTopic,
                                                     renderContentType)

-- Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = DbInitErr SQLiteResponse
  deriving Show

runApp :: IO ()
runApp = do
  -- Load our configuration
  cfgE <- prepareAppReqs
  -- Loading the configuration can fail, so we have to take that into account now.
  case cfgE of
    Left err   -> undefined
    Right _cfg -> run undefined undefined

-- We need to complete the following steps to prepare our app requirements:
--
-- 1) Load the configuration.
-- 2) Attempt to initialise the database.
-- 3) Combine the results into a tuple
--
-- The filename for our application config is: "files/appconfig.json"
--
prepareAppReqs
  :: IO ( Either StartUpError ( Conf, DB.FirstAppDB ) )
prepareAppReqs =
  error "copy your prepareAppReqs from the previous level."

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
  :: ToJSON a
  => a
  -> Response
resp200Json =
  resp200 JSON . A.encode
-- |

-- Now that we have our configuration, pass it where it needs to go.
app
  :: Conf
  -> DB.FirstAppDB
  -> Application
app cfg db rq cb =
  runAppM (handleRequest db =<< mkRequest rq) >>= cb . handleRespErr
  where
    handleRespErr :: Either Error Response -> Response
    handleRespErr = either mkErrorResponse id

-- Now we have some config, we can pull the ``helloMsg`` off it and use it in
-- the response.
handleRequest
  :: DB.FirstAppDB
  -> RqType
  -> AppM Response
handleRequest db rqType =
  -- Now that we're operating within the context of our AppM, which is a
  -- ReaderT, we're able to access the values stored in the Env.
  --
  -- Two functions that allow us to access the data stored in our ReaderT are:
  -- ask :: MonadReader r m => m r
  -- &
  -- asks :: MonadReader r m => (r -> a) -> m a
  --
  -- We will use ``asks`` here as we only want the FirstAppDB, so...
  -- > envDb      :: Env -> FirstAppDB
  -- > AppM       :: ReaderT Env IO a
  -- > asks       :: (Env -> a) -> AppM a
  -- > asks envDb :: AppM FirstAppDB
  case rqType of
    -- Exercise for later: Could this be generalised to clean up the repetition ?
    AddRq t c -> resp200 PlainText "Success" <$ DB.addCommentToTopic db t c
    ViewRq t  -> resp200Json <$> DB.getComments db t
    ListRq    -> resp200Json <$> DB.getTopics db

mkRequest
  :: Request
  -> AppM RqType
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
  resp500 PlainText "Oh noes"
