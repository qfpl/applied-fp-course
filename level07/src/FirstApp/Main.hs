{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main
  ( runApp
  , prepareAppReqs
  , app
  ) where

import           Control.Monad.Except               (ExceptT (ExceptT),
                                                     runExceptT)
import           Control.Monad.IO.Class             (liftIO)

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

import qualified FirstApp.DB                        as DB

import qualified FirstApp.Conf                      as Conf
import           FirstApp.Error                     (Error (DBError, EmptyCommentText, EmptyTopic, UnknownRoute))
import qualified FirstApp.Responses                 as Res
import           FirstApp.Types                     (Conf (..),
                                                     ConfigError (..),
                                                     RqType (AddRq, ListRq, ViewRq),
                                                     confPortToWai,
                                                     mkCommentText, mkTopic)

import           FirstApp.AppM                      (AppM,
                                                     Env (Env, envConfig, envDB),
                                                     runAppM, throwL)

-- Our start-up process is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = ConfErr ConfigError
  | DbInitErr SQLiteResponse
  deriving Show

runApp
  :: IO ()
runApp = do
  appE <- prepareAppReqs
  either print runWithDbConn appE
  where
    runWithDbConn env =
      appWithDb env >> DB.closeDB (envDB env)

    appWithDb env =
      run ( confPortToWai $ envConfig env ) (app env)

-- Monad transformers can be used without needing to write the newtype. Recall
-- that the constructor for ExceptT has a type of :: m (Either e a). So if you
-- have multiple functions that match that pattern and you don't want to have to
-- thread the error handling needle yourself. You can apply the constructor to
-- the functions and work directly on the values, knowing that the error
-- handling will work as expected. Then you `runExceptT` and produce the
-- final Either value.
prepareAppReqs
  :: IO (Either StartUpError Env)
prepareAppReqs =
  error "Copy your completed 'prepareAppReqs' and refactor to match the new type signature"
  where
    logToErr :: Text -> AppM ()
    logToErr = liftIO . hPutStrLn stderr

    toStartUpErr :: (a -> StartUpError) -> IO (Either a c) -> ExceptT StartUpError IO c
    toStartUpErr = error "toStartUpErr not reimplemented"

    -- Take our possibly failing configuration/db functions with their unique
    -- error types and turn them into a consistently typed ExceptT. We can then
    -- use them in a `do` block as if the Either isn't there. Extracting the
    -- final result before returning.
    initConf :: ExceptT StartUpError IO Conf
    initConf = toStartUpErr ConfErr $ Conf.parseOptions "appconfig.json"

    initDB :: Conf -> ExceptT StartUpError IO DB.FirstAppDB
    initDB cfg = toStartUpErr DbInitErr $ DB.initDB (dbFilePath cfg)

app
  :: Env
  -> Application
app env rq cb = do
  e <- requestToResponse
  resp <- either handleError pure e
  cb resp
  where
    logToErr :: Text -> IO ()
    logToErr = liftIO . hPutStrLn stderr

    requestToResponse :: IO (Either Error Response)
    requestToResponse = runAppM env $ mkRequest rq >>= handleRequest

    handleError :: Error -> IO Response
    handleError e = mkErrorResponse e <$ ( logToErr . Text.pack . show ) e

-- This function has changed quite a bit since we changed our DB functions to be
-- part of AppM. We no longer have to deal with the extra layer of the returned
-- Either and these functions share the same Monad, AppM.
handleRequest
  :: RqType
  -> AppM Response
handleRequest ( AddRq t c ) =
  -- We've cleaned this branch up a bit more by dropping our use of `const` as
  -- we can use the Functor operator that ignores the result on the right hand
  -- side and returns the result of the function on the left.
  Res.resp200 "Success" <$  DB.addCommentToTopic t c
handleRequest ( ViewRq t )  =
  Res.resp200Json       <$> DB.getComments t
handleRequest ListRq        =
  Res.resp200Json       <$> DB.getTopics

mkRequest
  :: Request
  -> AppM RqType
mkRequest rq =
  throwL =<< case ( pathInfo rq, requestMethod rq ) of
  -- Commenting on a given topic
  ( [t, "add"], "POST" ) ->
    liftIO $ mkAddRequest t <$> strictRequestBody rq
    -- View the comments on a given topic
  ( [t, "view"], "GET" ) ->
    pure ( mkViewRequest t )
  -- List the current topics
  ( ["list"], "GET" )    ->
    pure mkListRequest
  -- We don't care about any other requests so throw your hands in the air
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
mkErrorResponse UnknownRoute     =
  Res.resp404 "Unknown Route"
mkErrorResponse EmptyCommentText =
  Res.resp400 "Empty Comment"
mkErrorResponse EmptyTopic       =
  Res.resp400 "Empty Topic"
mkErrorResponse ( DBError _ )    =
  Res.resp500 "OH NOES"
