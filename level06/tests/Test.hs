{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad  (void, join)

import Data.Bifunctor (first)

import           Test.Hspec
import           Test.Hspec.Wai

import qualified System.Exit    as Exit

import qualified FirstApp.AppM  as AppM
import qualified FirstApp.DB    as DB
import qualified FirstApp.Main  as Main
import qualified FirstApp.Types as Types

main :: IO ()
main = do
  let dieWith m = print m >> Exit.exitFailure

  -- Keeping everything in sync with out larger application changes.
  reqsE <- Main.prepareAppReqs
  case reqsE of

    Left err -> dieWith err

    Right env -> do
      let app' = pure ( Main.app env )

          flushTopic :: IO ()
          flushTopic = AppM.runAppM env $ do
            r <- traverse DB.deleteTopic ( Types.mkTopic "fudge" )
            either ( liftIO . dieWith ) pure $ join r

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
