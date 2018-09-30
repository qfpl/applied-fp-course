module Helpers
  ( RequestPath (..)
  , TestM
  , get
  , post
  , put
  , runTestsFor
  , assertBody
  , assertStatus
  , assertContentType
  , rq
  , rqWithBody
  ) where

import           Test.Tasty.HUnit        (Assertion, assertFailure, (@=?))

import qualified System.Exit             as Exit

import qualified Control.Exception       as E

import           Control.Monad.Except    (ExceptT (..), runExceptT)
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Reader    (ReaderT (..), ask)
import           Control.Monad.State     (StateT (..), execStateT, lift)


import           Data.Semigroup          ((<>))

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS

import           Network.HTTP.Types      as HTTP

import           Network.Wai             (Application, Request (..))
import qualified Network.Wai.Test        as WT

import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar

type TestM = ReaderT (MVar String) WT.Session

newtype RequestPath = RequestPath
  { unRequestPath :: BS.ByteString
  }

rq :: StdMethod -> RequestPath -> Request
rq mth rpath = flip WT.setPath (unRequestPath rpath) $ WT.defaultRequest
  { requestMethod = HTTP.renderStdMethod mth
  }

rqWithBody
  :: StdMethod
  -> RequestPath
  -> LBS.ByteString
  -> WT.SRequest
rqWithBody mth rpath =
  WT.SRequest (rq mth rpath)

runTestsFor :: Application -> TestM a -> IO a
runTestsFor app m = do
  testMV <- MVar.newEmptyMVar
  a      <- runSess testMV
  test   <- MVar.readMVar testMV
  either (sad test) (const Exit.exitSuccess) a
  where
    runSess mv = E.try
      $ WT.runSession (runReaderT m mv) app

    sad test (WT.WaiTestFailure msg) = do
      putStrLn $ "\tTest Failure " <> test
      putStrLn $ "\t" <> msg
      Exit.exitFailure

testRequest :: String -> WT.Session a -> TestM a
testRequest test r = ask >>= liftIO . flip MVar.putMVar test >> lift r

get :: String -> BS.ByteString -> TestM WT.SResponse
get test = testRequest test . WT.request . rq HTTP.GET . RequestPath

post :: String -> BS.ByteString -> LBS.ByteString -> TestM WT.SResponse
post test r = testRequest test . WT.srequest . rqWithBody HTTP.POST (RequestPath r)

put :: String -> BS.ByteString -> LBS.ByteString -> TestM WT.SResponse
put test r = testRequest test . WT.srequest . rqWithBody HTTP.PUT (RequestPath r)

assertBody :: LBS.ByteString -> WT.SResponse -> TestM ()
assertBody b = lift . WT.assertBody b

assertStatus :: HTTP.Status -> WT.SResponse -> TestM ()
assertStatus c = lift . WT.assertStatus (HTTP.statusCode c)

assertContentType :: BS.ByteString -> WT.SResponse -> TestM ()
assertContentType b = lift . WT.assertContentType b
