{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad  (join)

import           Test.Hspec
import           Test.Hspec.Wai

import qualified System.Exit    as Exit

import qualified FirstApp.DB    as DB
import qualified FirstApp.Main  as Main
import qualified FirstApp.Types as Types

main :: IO ()
main = do
  let dieWith m = print m >> Exit.exitFailure

  reqsE <- Main.prepareAppReqs
  case reqsE of

    Left err -> dieWith err

    Right ( cfg, db ) -> do
      let app' = pure ( Main.app cfg db )

          flushTopic =
            -- Clean up and yell about our errors
            fmap ( either dieWith pure . join ) .
            -- Purge all of the comments for this topic for our tests
            traverse ( DB.deleteTopic db )
            -- We don't export the constructor so even for known values we have
            -- to play by the rules. There is no - "Oh just this one time.", do it right.
            $ Types.mkTopic "fudge"

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
