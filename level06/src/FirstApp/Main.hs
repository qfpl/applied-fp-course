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

import           Network.Wai                        (Application, Request,
                                                     Response, pathInfo,
                                                     requestMethod,
                                                     strictRequestBody)
import           Network.Wai.Handler.Warp           (run)

import           Data.Bifunctor                     (first)
import           Data.Either                        (Either (Left, Right),
                                                     either)

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
import           FirstApp.Types                     (Conf (dbFilePath),
                                                     ConfigError,
                                                     ContentType (PlainText),
                                                     Error (DBError, EmptyCommentText, EmptyTopic, UnknownRoute),
                                                     RqType (AddRq, ListRq, ViewRq),
                                                     confPortToWai,
                                                     mkCommentText, mkTopic)

import           FirstApp.AppM                      (AppM, Env (Env, envConfig, envDB, envLoggingFn))

-- Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = ConfErr ConfigError
  | DbInitErr SQLiteResponse
  deriving Show

runApp :: IO ()
runApp = do
  appE <- prepareAppReqs
  either print runWithDbConn appE
  where
    runWithDbConn env =
      appWithDb env >> DB.closeDB (envDB env)

    appWithDb env =
      run ( confPortToWai $ envConfig env ) (app env)

prepareAppReqs
  :: IO (Either StartUpError Env)
prepareAppReqs = do
  cfgE <- initConf
  -- This is awkward because we need to initialise our DB using the config,
  -- which might have failed to be created for some reason, but our DB start up
  -- might have also failed for some reason. This is a bit clunky
  dbE <- join <$> traverse initDB cfgE

  -- Wrap our values (if we have them) in our Env for use in other parts of our
  -- application. We do it this way so we can have access to the bits we need
  -- when starting up the full app or one for testing.
  pure $ liftA2 ( Env logToErr ) cfgE dbE
  where
    logToErr = liftIO . hPutStrLn stderr

    -- This makes it a bit easier to take our individual initialisation
    -- functions and ensure that they both conform to the StartUpError type
    -- that we want them too.
    --
    -- Fun for later: Play with composing 'fmap' and 'first' in ghci and
    -- watch how the types specialise and change.
    toStartUpErr :: (a -> b) -> IO (Either a c) -> IO (Either b c)
    toStartUpErr = fmap . first

    -- Prepare the config
    initConf :: IO (Either StartUpError Conf)
    initConf = toStartUpErr ConfErr $ Conf.parseOptions "appconfig.json"

    -- Power up the tubes
    initDB :: Conf -> IO (Either StartUpError DB.FirstAppDB)
    initDB cfg = toStartUpErr DbInitErr $ DB.initDB (dbFilePath cfg)

-- Now that our request handling and response creating functions operate
-- within our AppM context, we need to run the AppM to get our IO action out
-- to be run and handed off to the callback function. We've already written
-- the function for this so include the 'runAppM' with the Env.
app
  :: Env
  -> Application
app =
  error "Copy your completed 'app' from the previous level and refactor it here"

handleRequest
  :: RqType
  -> AppM (Either Error Response)
handleRequest rqType =
  case rqType of
    -- Exercise: Could this be generalised to clean up the repetition ?
    AddRq t c -> pure (Res.resp200 PlainText "Success") <$ DB.addCommentToTopic t c
    ViewRq t  -> fmap Res.resp200Json <$> DB.getComments t
    ListRq    -> fmap Res.resp200Json <$> DB.getTopics

mkRequest
  :: Request
  -- We change this to be in our AppM context as well because when we're
  -- constructing our RqType we might want to call on settings or other such
  -- things, maybe.
  -> AppM ( Either Error RqType )
mkRequest rq =
  case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) ->
      liftIO (mkAddRequest t <$> strictRequestBody rq)
    -- View the comments on a given topic
    ( [t, "view"], "GET" ) -> pure ( mkViewRequest t )
    -- List the current topics
    ( ["list"], "GET" )    -> pure mkListRequest
    -- Finally we don't care about any other requests so throw your hands in the air
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
  -> AppM Response
mkErrorResponse UnknownRoute     =
  pure $ Res.resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText =
  pure $ Res.resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic       =
  pure $ Res.resp400 PlainText "Empty Topic"
mkErrorResponse ( DBError _e )    = do
  -- As with our request for the FirstAppDB, we use the asks function from
  -- Control.Monad.Reader and pass the field accessors from the Env record.
  error "mkErrorResponse needs to 'log' our DB Errors to the console"
  -- Be a sensible developer and don't leak your DB errors over the internet.
  pure (Res.resp500 PlainText "OH NOES")
