{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main
  ( runApp
  , prepareAppReqs
  , app
  ) where

import           Control.Applicative                (liftA2)
import           Control.Monad                      (join)

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Network.Wai
import           Network.Wai.Handler.Warp           (run)

import           Data.Bifunctor                     (first)
import           Data.Either                        (Either (..), either)

import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Text.Encoding                 (decodeUtf8)
import           Data.Text.IO                       (hPutStrLn)

import qualified Data.ByteString.Lazy.Char8         as LBS

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           System.IO                          (stderr)

import qualified FirstApp.Conf                      as Conf
import qualified FirstApp.DB                        as DB

import qualified FirstApp.Responses                 as Res
import           FirstApp.Types

import           FirstApp.AppM

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
    runWithDbConn env =
      appWithDb env >> DB.closeDb (envDb env)

    appWithDb env =
      run ( Conf.getPort . Conf.port $ envConfig env) (app env)

prepareAppReqs
  :: IO (Either StartUpError Env)
prepareAppReqs = do
  cfgE <- initConf
  -- This is awkward because we need to initialise our DB using the config,
  -- which might have failed to be created for some reason, but our DB start up
  -- might have also failed for some reason. This is a bit clunky
  dbE <- fmap join $ traverse initDB cfgE
  -- Wrap our values (if we have them) in our Env for use in other parts of our
  -- application. We do it this way so we can have access to the bits we need
  -- when starting up the full app or one for testing.
  pure $ liftA2 ( Env logToErr ) cfgE dbE
  where
    logToErr = liftIO . hPutStrLn stderr

    toStartUpErr e =
      -- This just makes it a bit easier to take our individual initialisation
      -- functions and ensure that they both conform to the StartUpError type
      -- that we want them too.
      fmap (first e)

    initConf = toStartUpErr ConfErr
      -- Prepare the configgening
      $ Conf.parseOptions "appconfig.json"

    initDB cfg = toStartUpErr DbInitErr
      -- Power up the tubes
      $ DB.initDb (Conf.dbFilePath cfg) (Conf.tableName cfg)

app
  :: Env
  -> Application
app env rq cb =
  requestToResponse >>= cb
  where
    -- Now that our request handling and response creating functions operate
    -- within our AppM context, we need to run the AppM to get our IO action out
    -- to be run and handed off to the callback function. We've already written
    -- the function for this so include the 'runAppM' with the Env.
    requestToResponse = runAppM env $ do
      -- Exercise: Rewrite this function to remove the need for the intermediate values.
      rq' <- mkRequest rq
      er' <- handleRErr rq'
      handleRespErr er'

    handleRespErr =
      either mkErrorResponse pure

    handleRErr =
      either ( pure . Left ) handleRequest

handleRequest
  :: RqType
  -> AppM (Either Error Response)
handleRequest rqType = do
  -- Now that we're operating within the context of our AppM, which is a
  -- ReaderT, we're able to access the values stored in the Env.
  --
  -- Two functions that allow us to access the data stored in our ReaderT are:
  -- ask :: MonadReader r m => m r
  -- &
  -- asks :: MonadReader r m => (r -> a) -> m a
  --
  -- We will use asks here as we're only after the FirstAppDB, so...
  -- > envDb      :: Env -> FirstAppDB
  -- > AppM       :: ReaderT Env IO a
  -- > asks       :: (Env -> a) -> AppM a
  -- > asks envDb :: AppM FirstAppDB
  db <- asks envDb
  liftIO $ case rqType of
    -- Exercise: Could this be generalised to clean up the repetition ?
    AddRq t c -> fmap (const ( Res.resp200 "Success" )) <$> DB.addCommentToTopic db t c
    ViewRq t  -> fmap Res.resp200Json <$> DB.getComments db t
    ListRq    -> fmap Res.resp200Json <$> DB.getTopics db

mkRequest
  :: Request
  -- We change this to be in our AppM context as well because when we're
  -- constructing our RqType we might want to call on settings or other such
  -- things, maybe.
  -> AppM ( Either Error RqType )
mkRequest rq =
  case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) -> liftIO $ mkAddRequest t <$> strictRequestBody rq
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
  -> AppM Response
mkErrorResponse UnknownRoute     = pure $ Res.resp404 "Unknown Route"
mkErrorResponse EmptyCommentText = pure $ Res.resp400 "Empty Comment"
mkErrorResponse EmptyTopic       = pure $ Res.resp400 "Empty Topic"
mkErrorResponse ( DBError e )    = do
  -- As with our request for the FirstAppDB, we use the asks function from
  -- Control.Monad.Reader and pass the field accessor from the Env record.
  rick <- asks loggingRick
  rick . Text.pack $ show e
  -- Be a sensible developer and don't leak your DB errors over the interwebs.
  pure $ Res.resp500 "OH NOES"
