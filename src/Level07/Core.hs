{-# LANGUAGE OverloadedStrings #-}
module Level07.Core
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

import qualified Level07.Conf                       as Conf
import qualified Level07.DB                         as DB

import qualified Level07.Responses                  as Res
import           Level07.Types                      (Conf (dbFilePath),
                                                     ConfigError,
                                                     ContentType (PlainText),
                                                     Error (DBError, EmptyCommentText, EmptyTopic, UnknownRoute),
                                                     RqType (AddRq, ListRq, ViewRq),
                                                     confPortToWai,
                                                     mkCommentText, mkTopic)

import           Level07.AppM                       (AppM, Env (Env, envConfig, envDB, envLoggingFn),
                                                     liftEither)

-- We're going to use the `mtl` ExceptT monad transformer to make the loading of our `Conf` a bit more straight-forward.
import           Control.Monad.Except               (ExceptT (..), runExceptT)

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

-- Reimplement the `prepareAppReqs` function using the imported `ExceptT`
-- constructor to help eliminate the manual plumbing of the error values.
--
-- We'll use the more general version of our error handling monad transformer to
-- demonstrate how easily it can be applied simplify error handling.
prepareAppReqs
  :: IO (Either StartUpError Env)
prepareAppReqs = runExceptT $
  error "Copy your completed 'prepareAppReqs' from the previous level and refactor it here"

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
  -> AppM Response
handleRequest rqType = case rqType of
  AddRq t c -> Res.resp200 PlainText "Success" <$ DB.addCommentToTopic t c
  ViewRq t  -> Res.resp200Json <$> DB.getComments t
  ListRq    -> Res.resp200Json <$> DB.getTopics

mkRequest
  :: Request
  -> AppM RqType
mkRequest rq =
  liftEither =<< case ( pathInfo rq, requestMethod rq ) of
    -- Commenting on a given topic
    ( [t, "add"], "POST" ) -> liftIO (mkAddRequest t <$> strictRequestBody rq)
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
  -> Response
mkErrorResponse UnknownRoute     =
  Res.resp404 PlainText "Unknown Route"
mkErrorResponse EmptyCommentText =
  Res.resp400 PlainText "Empty Comment"
mkErrorResponse EmptyTopic       =
  Res.resp400 PlainText "Empty Topic"
mkErrorResponse ( DBError _ )    =
  -- Be a sensible developer and don't leak your DB errors over the internet.
  Res.resp500 PlainText "OH NOES"
