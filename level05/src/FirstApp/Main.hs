{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main
  ( runApp
  , prepareAppReqs
  , app
  ) where

import           Control.Applicative                (liftA2)
import           Control.Exception                  (bracket)
import           Control.Monad                      (join)

import           Network.Wai
import           Network.Wai.Handler.Warp           (run)

import           Network.HTTP.Types                 (Status, hContentType,
                                                     status200, status400,
                                                     status404, status500)

import qualified Data.ByteString.Lazy.Char8         as LBS

import           Data.Either                        (Either (..), either)

import           Data.Semigroup                     ((<>))
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8)

import           Data.Aeson                         (ToJSON)
import qualified Data.Aeson                         as A

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import qualified FirstApp.Conf                      as Conf
import qualified FirstApp.DB                        as DB
import           FirstApp.Types

-- Our startup is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire startup process as a whole.
data StartUpError
  = ConfErr Conf.ConfigError
  | DbInitErr SQLiteResponse
  deriving Show

runApp :: IO ()
runApp = do
  appE <- prepareAppReqs
  either print runWithDbConn appE
  where
    runWithDbConn ( cfg, db ) =
      -- The bracket function will take care of closing our DB connection in the
      -- event of an application collapse. It is a very useful function for
      -- managing resources and deferred actions.
      bracket (pure db) DB.closeDb (appWithDb cfg)

    appWithDb cfg db =
      -- Just a helper to actually use the Wai function to run out fully
      -- realised app function.
      run ( Conf.getPort $ Conf.port cfg ) $ app cfg db

prepareAppReqs
  :: IO (Either StartUpError (Conf.Conf,DB.FirstAppDB))
prepareAppReqs = do
  cfgE <- initConf
  -- This is awkward because we need to initialise our DB using the config,
  -- which might have failed to be created for some reason, but our DB start up
  -- might have also failed for some reason. This is a bit clunky
  dbE <- fmap join $ traverse initDB cfgE
  -- Wrap our values (if we have them) in a tuple for use in other parts of our
  -- application. We do it this way so we can have access to the bits we need
  -- when starting up the full app or one for testing.
  pure $ liftA2 (,) cfgE dbE
  where
    toStartUpErr e =
      -- This just makes it a bit easier to take our individual initialisation
      -- functions and ensure that they both conform to the StartUpError type
      -- that we want them too.
      fmap ( either (Left . e) Right )

    initConf = toStartUpErr ConfErr
      -- Prepare the configgening
      $ Conf.parseOptions "appconfig.json"

    initDB cfg = toStartUpErr DbInitErr
      -- Power up the tubes
      $ DB.initDb (Conf.dbFilePath cfg) (Conf.tableName cfg)

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

-- Some new helpers for different statuses and content types
resp500
  :: LBS.ByteString
  -> Response
resp500 =
  mkResponse status500 PlainText

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
app cfg db rq cb = do
  rq' <- mkRequest rq
  resp <- fmap handleRespErr $ handleRErr rq'
  cb resp
  where
    -- Does this seem clunky to you?
    handleRespErr = either mkErrorResponse id
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
  fmap (const ( resp200 "Success" )) <$> DB.addCommentToTopic db t c
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
mkErrorResponse ( DBError e ) =
  -- If a DB error happens, it's going to be sad town, population you. But you
  -- should let someone know. How we go about changing this function to include logging ?
  resp500 . LBS.pack $ "Database Error" <> show e
