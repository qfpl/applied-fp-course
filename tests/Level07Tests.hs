{-# LANGUAGE OverloadedStrings #-}
module Level07Tests
  ( unitTests
  , doctests
  ) where

import           Control.Monad.Reader (ask, reader)

import           Control.Monad        (join)

import           Test.Hspec
import           Test.Hspec.Wai

import qualified System.Exit          as Exit

import           Level07.AppM         (Env)
import qualified Level07.AppM         as AppM

import qualified Level07.Core         as Core
import qualified Level07.DB           as DB
import qualified Level07.Types        as Types

doctests :: [FilePath]
doctests =
  [ "-isrc"
  , "src/FirstApp/Conf.hs"
  , "src/FirstApp/DB.hs"
  , "src/FirstApp/Types.hs"
  ]

unitTests :: IO ()
unitTests = do
  let dieWith m = print m >> Exit.exitFailure

  -- Keeping everything in sync with out larger application changes.
  reqsE <- Core.prepareAppReqs
  case reqsE of

    Left err -> dieWith err

    Right env -> do
      let app' = pure ( Core.app env )

          flushTopic :: IO ()
          flushTopic = either (liftIO . dieWith) pure =<< AppM.runAppM
            (AppM.liftEither =<< traverse DB.deleteTopic ( Types.mkTopic "fudge" ))
            env

      -- We can't run the tests for our AppM in the same stage as our
      -- application, because of the use of the 'with' function. As it expects
      -- to be able to execute our tests by applying it to our 'Application'.
      hspec $ appMTests env

      -- Run the tests with a DB topic flush between each spec
      hspec . with ( flushTopic >> app' ) $ do

        -- AddRq Spec
        describe "POST /topic/add" $ do

          it "Should return 200 with well formed request" $
            post "/fudge/add" "Fred" `shouldRespondWith` "Success"

          it "Should 400 on empty input" $
            post "/fudge/add" "" `shouldRespondWith` 400

        -- ViewRq Spec
        describe "GET /topic/view" $
          it "Should return 200 with content" $ do
            post "/fudge/add" "Is super tasty."
            get "/fudge/view" `shouldRespondWith` 200

        -- ListRq Spec
        describe "GET /list" $
          it "Should return 200 with content" $ do
            post "/fudge/add" "Is super tasty."
            get "/list" `shouldRespondWith` "[\"fudge\"]"


-- These tests ensure that our AppM will do we want it to, with respect to the
-- behaviour of 'ask', 'reader', and use in a Monad.
appMTests :: Env -> Spec
appMTests env = describe "AppM Tests" $ do

  it "ask should retrieve the Env" $ do
    r <- AppM.runAppM ask env
    ( (AppM.envConfig <$> r) == (Right $ AppM.envConfig env) ) `shouldBe` True

  it "reader should run a function on the Env" $ do
    let getDBfilepath = Types.dbFilePath . AppM.envConfig

    r <- AppM.runAppM ( reader getDBfilepath ) env
    r `shouldBe` (Right $ getDBfilepath env)

  it "should let us run IO functions" $ do
    let fn = do
          e <- ask
          AppM.envLoggingFn e "In a test!"
    r <- AppM.runAppM fn env
    r `shouldBe` (Right ())
