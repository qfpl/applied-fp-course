{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Monoid          ((<>))

import           Data.String          (IsString)

import           Test.Hspec
import           Test.Hspec.Wai

import qualified System.Exit          as Exit

import qualified FirstApp.AppM        as AppM
import qualified FirstApp.DB          as DB
import qualified FirstApp.Main        as Main
import qualified FirstApp.Types       as Types

main :: IO ()
main = do
  let
    dieWith :: Show a => a -> IO ()
    dieWith m = print m >> Exit.exitFailure

    -- This helps keep the string polymorphic so we can use it in both
    -- ByteString and Text forms in this file, without having to run encoding
    -- functions. The compiler takes care of it for us.
    testTopic :: IsString s => s
    testTopic = "fudge"

  reqsE <- Main.prepareAppReqs
  case reqsE of

    Left err -> dieWith err

    Right env -> do
      let app' = pure ( Main.app env )

          flushTopic :: IO ()
          flushTopic = (either dieWith pure =<<)
            . AppM.runAppM env $ do
                t <- AppM.throwL $ Types.mkTopic "fudge"
                DB.deleteTopic t

      -- Run the tests with a DB topic flush between each spec
      hspec . with ( flushTopic >> app' ) $ do
        -- Save us a bit of repetition
        let pOST = post ( "/" <> testTopic <> "/add" )

        -- AddRq Spec
        describe "POST /topic/add" $ do
          it "Should return 200 with well formed request" $
            pOST "Is super tasty." `shouldRespondWith` "Success"

          it "Should 400 on empty input" $
            pOST "" `shouldRespondWith` 400

        -- ViewRq Spec
        describe "GET /topic/view" $
          it "Should return 200 with content" $ do
            _ <- pOST "Is super tasty."
            get ( "/" <> testTopic <> "/view" ) `shouldRespondWith` 200

        -- ListRq Spec
        describe "GET /list" $
          it "Should return 200 with content" $ do
            _ <- pOST "Is super tasty."
            get "/list" `shouldRespondWith` "[\"fudge\"]"
