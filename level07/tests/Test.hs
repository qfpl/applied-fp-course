{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Monoid    ((<>))

import           Data.String    (IsString)

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

      -- This just helps keep the string polymorphic so we can use it in both
      -- ByteString and Text forms in this file, without having to run encoding
      -- functions. The compiler takes care of it for us.
      testTopic :: IsString s => s
      testTopic = "fudge"

  reqsE <- Main.prepareAppReqs
  case reqsE of

    Left err -> dieWith err

    Right env -> do
      let app' = pure ( Main.app env )

          -- Keeping in line with the changes to our application to use a AppM,
          -- and now wrapping our DB layer in the AppM as well. This function
          -- needs an overhaul.
          flushTopic = do
            error "flushTopic not reimplemented"
            -- To lift our AppM into the base IO, we run it as would if it were
            -- a normal AppM, returning our IO ( Either ) result.
            -- r <- AppM.runAppM env $ do
              -- This inner 'do' is running as if it were an AppM, including all of the nice error handling etc.
              -- t <- AppM.throwL $ Types.mkTopic testTopic
              -- DB.deleteTopic t
            -- This outer 'do' is in the base IO monad and if we have a failure
            -- here we need to exit with an error code to ensure the test-suite
            -- knows to fail our tests.
            -- either dieWith pure r

      -- Run the tests with a DB topic flush between each spec
      hspec . with ( flushTopic >> app' ) $ do
        -- Save us a bit of repetition
        let pOST = post ( "/" <> testTopic <> "/add" )

        -- AddRq Spec
        describe "POST /topic/add" $ do
          it "Should return 200 with well formed request" $ do
            pOST "Fred" `shouldRespondWith` "Success"

          it "Should 400 on empty input" $
            pOST "" `shouldRespondWith` 400

        -- ViewRq Spec
        describe "GET /topic/view" $ do
          it "Should return 200 with content" $ do
            _ <- pOST "Fred"
            get ( "/" <> testTopic <> "/view" ) `shouldRespondWith` 200

        -- ListRq Spec
        describe "GET /list" $ do
          it "Should return 200 with content" $ do
            _ <- pOST "Fred"
            get "/list" `shouldRespondWith` "[\"fudge\"]"
