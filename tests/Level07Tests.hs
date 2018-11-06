{-# LANGUAGE OverloadedStrings #-}
module Level07Tests
  ( unitTests
  , doctests
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ask, reader)

import           Control.Monad          (join)

import           Data.Foldable          (traverse_)
import           Data.Monoid            ((<>))
import           Data.String            (IsString)

import           Test.Hspec

import           Network.HTTP.Types     as HTTP

import           Helpers                (TestM, assertBody, assertStatus, get,
                                         post, runTestsFor)

import qualified System.Exit            as Exit

import           Level07.AppM           (Env)
import qualified Level07.AppM           as AppM

import qualified Level07.Core           as Core
import qualified Level07.DB             as DB
import qualified Level07.Types          as Types

doctests :: [FilePath]
doctests =
  [ "-isrc"
  , "src/Level07/Conf.hs"
  , "src/Level07/DB.hs"
  , "src/Level07/Types.hs"
  ]

unitTests :: IO ()
unitTests = do
  let
    dieWith :: Show a => a -> IO ()
    dieWith m = print m >> Exit.exitFailure

    testTopic :: IsString s => s
    testTopic = "fudge"

  reqsE <- Core.prepareAppReqs
  case reqsE of
    Left err -> dieWith err
    Right e -> runTestsFor (Core.app e) "Level 07 API Tests" $ do

      let
        flushTopic :: TestM ()
        flushTopic = liftIO .
          -- Clean up and yell about our errors
          (traverse_ (either dieWith pure) =<<) .
          -- Include the runner to handle our new 'AppM'
          flip AppM.runAppM e .
          -- Purge all of the comments for this topic for our tests
          traverse DB.deleteTopic
          -- We don't export the constructor so even for known values we have
          -- to play by the rules. There is no - "Oh just this one time.", do it right.
          $ Types.mkTopic "fudge"

        -- Run a test and then flush the db
        test t = t >> flushTopic

        topicR = "/fudge/"

        addToTopic =
          post "Add Topic" (topicR <> "add") "Fred"

      -- AddRq Spec
      -- it should return 200 with well formed request
      test $ addToTopic >>= assertBody "Success"

      -- it should 400 on empty input
      test $ post "Empty Input" (topicR <> "add") ""
        >>= assertStatus HTTP.status400

      -- ViewRq Spec
      -- it should return 200 with
      test $ addToTopic
        >> get "View topic" (topicR <> "view")
        >>= assertStatus HTTP.status200

      -- ListRq Spec
      test $ addToTopic
        >> get "List topics" "/list"
        >>= assertBody "[\"fudge\"]"

-- These tests ensure that our AppM will do we want it to, with respect to the
-- behaviour of 'ask', 'reader', and use in a Monad.
appMTests :: Env -> Spec
appMTests env = describe "AppM Tests" $ do

  it "ask should retrieve the Env" $ do
    r <- AppM.runAppM ask env
    ( (AppM.envConfig <$> r) == Right (AppM.envConfig env) ) `shouldBe` True

  it "reader should run a function on the Env" $ do
    let getDBfilepath = Types.dbFilePath . AppM.envConfig

    r <- AppM.runAppM ( reader getDBfilepath ) env
    r `shouldBe` Right (getDBfilepath env)

  it "should let us run IO functions" $ do
    let fn = do
          e <- ask
          AppM.envLoggingFn e "In a test!"
    r <- AppM.runAppM fn env
    r `shouldBe` Right ()
