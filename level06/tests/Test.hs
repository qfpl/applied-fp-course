{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad  (join)

import           Test.Hspec
import           Test.Hspec.Wai

import qualified System.Exit    as Exit

import qualified FirstApp.Conf  as Conf
import qualified FirstApp.DB    as DB
import qualified FirstApp.Main  as Main
import qualified FirstApp.Types as Types
import qualified FirstApp.AppM  as AppM
main :: IO ()
main = do
  let dieWith m = print m >> Exit.exitFailure

  -- Keeping everything in sync with out larger application changes.
  reqsE <- Main.prepareAppReqs
  case reqsE of

    Left err -> dieWith err

    Right env -> do
      let app' = pure ( Main.app env )

          -- Write a function to clear the comments for a specific topic.
          -- This will be run before each test is run.
          flushTopic =
            error "Flush topic not implemented"

      -- Run the tests with a DB topic flush between each spec
      hspec . with ( flushTopic >> app' ) $ do

        -- AddRq Spec
        describe "POST /topic/add" $ do

          it "Should return 200 with well formed request" $ do
            post "/fudge/add" "Fred" `shouldRespondWith` "Success"

          it "Should 400 on empty input" $
            post "/fudge/add" "" `shouldRespondWith` 400

        -- ViewRq Spec
        describe "GET /topic/view" $ do
          it "Should return 200 with content" $ do
            post "/fudge/add" "Is super tasty."
            get "/fudge/view" `shouldRespondWith` 200

        -- ListRq Spec
        describe "GET /list" $ do
          it "Should return 200 with content" $ do
            post "/fudge/add" "Is super tasty."
            get "/list" `shouldRespondWith` "[\"fudge\"]"
