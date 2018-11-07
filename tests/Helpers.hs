module Helpers
  ( -- * Test Monad
    TestM

    -- * Test Runner
  , runTestsFor

    -- * Request Builders
  , get
  , post
  , put

    -- * Response Assertions
  , assertBody
  , assertStatus
  , assertContentType

    -- * Internals
  , RequestPath (..)
  , rq
  , rqWithBody

  ) where

import qualified System.Exit               as Exit

import qualified Control.Exception         as E

import           Control.Monad.Except      (ExceptT (..), runExceptT)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Reader      (ReaderT (..), ask, runReaderT)
import           Control.Monad.State       (StateT (..), evalStateT, lift,
                                            runStateT)
import qualified Control.Monad.State       as State

import           Data.Semigroup            ((<>))

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS

import           Network.HTTP.Types        as HTTP

import           Network.Wai               (Application, Request (..))
import           Network.Wai.Test          (Session, WaiTestFailure (..))
import qualified Network.Wai.Test          as WT
import           Network.Wai.Test.Internal (ClientState, initState)

import           Control.Concurrent.MVar   (MVar)
import qualified Control.Concurrent.MVar   as MVar

import           Control.Monad.Morph       (hoist)

-- | This terrifying beast is the combination of the 'Session' transformer stack
-- from Wai.Test and the transformer stack required to keep track of the test
-- name, along with catching the exceptions so we don't just die with an awful
-- failure and no information.
type TestM = ReaderT Application (StateT ClientState (ExceptT WaiTestFailure (StateT String IO)))

-- | By leaning on some monad morphisms, we're able to insert a transformer
-- stack at an arbitrary point in a different transformer stack that we do not
-- control. This lets us extend it with new functionality that it may not have been
-- designed for.
manipulateTransStack :: WT.Session a -> TestM a
manipulateTransStack = hoist (hoist (hoist lift . ExceptT . E.try))

-- | Although not exported, this newtype helps us keep our strings in line.
newtype RequestPath = RequestPath
  { unRequestPath :: BS.ByteString
  }

-- | Create an empty 'Request' using the given HTTP Method and route.
rq :: StdMethod -> RequestPath -> Request
rq mth rpath = flip WT.setPath (unRequestPath rpath) $ WT.defaultRequest
  { requestMethod = HTTP.renderStdMethod mth
  }

-- | Create a 'Request' with a body.
rqWithBody
  :: StdMethod
  -> RequestPath
  -> LBS.ByteString
  -> WT.SRequest
rqWithBody mth rpath =
  WT.SRequest (rq mth rpath)

-- | Run a single instance of the 'Application' for all of the tests given in the 'TestM'.
runTestsFor :: Application -> String -> TestM a -> IO a
runTestsFor app nm m = do
  (e, testName) <- runStateT (runExceptT (evalStateT (runReaderT m app) initState)) nm
  either (sad testName) pure e
  where
    sad test (WT.WaiTestFailure msg) = do
      putStrLn $ "\tTest Failure For: " <> "[" <> test <> "]"
      putStrLn $ "\t" <> msg
      Exit.exitFailure

testRequest :: String -> WT.Session a -> TestM a
testRequest test r = do
  lift . lift . State.put $ test
  manipulateTransStack r

get :: String -> BS.ByteString -> TestM WT.SResponse
get test = testRequest test . WT.request . rq HTTP.GET . RequestPath

post :: String -> BS.ByteString -> LBS.ByteString -> TestM WT.SResponse
post test r = testRequest test . WT.srequest . rqWithBody HTTP.POST (RequestPath r)

put :: String -> BS.ByteString -> LBS.ByteString -> TestM WT.SResponse
put test r = testRequest test . WT.srequest . rqWithBody HTTP.PUT (RequestPath r)

assertBody :: LBS.ByteString -> WT.SResponse -> TestM ()
assertBody b = manipulateTransStack . WT.assertBody b

assertStatus :: HTTP.Status -> WT.SResponse -> TestM ()
assertStatus c = manipulateTransStack . WT.assertStatus (HTTP.statusCode c)

assertContentType :: BS.ByteString -> WT.SResponse -> TestM ()
assertContentType b = manipulateTransStack . WT.assertContentType b
