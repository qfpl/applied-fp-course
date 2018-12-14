{-# LANGUAGE OverloadedStrings #-}
module Level07.Core
  ( runApplication
  , prepareAppReqs
  , app
  ) where

import           Control.Applicative                (liftA2)
import qualified Control.Exception                  as Ex
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

import qualified Waargonaut.Encode                  as E

import qualified Level07.Conf                       as Conf
import qualified Level07.DB                         as DB

import qualified Level07.Responses                  as Res
import           Level07.Types                      (Conf, ConfigError,
                                                     ContentType (PlainText),
                                                     Error (..), RqType (..),
                                                     confPortToWai,
                                                     encodeComment, encodeTopic,
                                                     mkCommentText, mkTopic)

import           Level07.AppM                       (App, Env (..), liftEither,
                                                     runApp)

-- | We're going to use the `mtl` ExceptT monad transformer to make the loading of
-- our `Conf` a bit more straight-forward.
import           Control.Monad.Except               (ExceptT (..), runExceptT)

-- | Our start-up is becoming more complicated and could fail in new and
-- interesting ways. But we also want to be able to capture these errors in a
-- single type so that we can deal with the entire start-up process as a whole.
data StartUpError
  = DBInitErr SQLiteResponse
  | ConfErr ConfigError
  deriving Show

runApplication :: IO ()
runApplication = do
  appE <- runExceptT prepareAppReqs
  either print runWithDBConn appE
  where
    runWithDBConn env =
      appWithDB env >> DB.closeDB (envDB env)

    appWithDB env = Ex.finally
      (run ( confPortToWai . envConfig $ env ) (app env))
      $ DB.closeDB (envDB env)

-- | Our AppM is no longer useful for implementing this function. Can you explain why?
--
-- We will reimplement this function using `ExceptT`. It is from the 'mtl'
-- package and it's the very general form of the AppM we implemented previously.
-- It has all of the useful instances written for us, along with many utility
-- functions.
--
-- 'mtl' on Hackage: https://hackage.haskell.org/package/mtl
--
prepareAppReqs :: ExceptT StartUpError IO Env
prepareAppReqs = error "prepareAppReqs not reimplemented with ExceptT"
  -- You may copy your previous implementation of this function and try refactoring it. On the
  -- condition you have to explain to the person next to you what you've done and why it works.

-- | Now that our request handling and response creating functions operate
-- within our App context, we need to run the App to get our IO action out
-- to be run and handed off to the callback function. We've already written
-- the function for this so include the 'runApp' with the Env.
app
  :: Env
  -> Application
app =
  error "Copy your completed 'app' from the previous level and refactor it here"

handleRequest
  :: RqType
  -> App Response
handleRequest rqType = case rqType of
  AddRq t c -> Res.resp200 PlainText "Success" <$ DB.addCommentToTopic t c
  ViewRq t  -> Res.resp200Json (E.list encodeComment) <$> DB.getComments t
  ListRq    -> Res.resp200Json (E.list encodeTopic)   <$> DB.getTopics

mkRequest
  :: Request
  -> App RqType
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
