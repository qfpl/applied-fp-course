{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Test.Hspec
import           Test.Hspec.Wai

import qualified System.Exit                as Exit

import qualified Data.ByteString.Lazy.Char8 as LBS8
import           Data.String                (fromString)

import qualified FirstApp.Conf              as Conf
import qualified FirstApp.Main              as Main

main :: IO ()
main = do
  cfgE <- Conf.parseOptions "appconfig.json"
  case cfgE of

    Left err -> do
      print err
      Exit.exitFailure

    Right cfg -> do
      let app' = pure ( Main.app cfg )
          -- The hspec-wai package uses the IsString typeclass to be able to turn
          -- String values into a ResponseMatcher record type. It's not terribly
          -- transparent or elegant, but it works sufficiently well for these tests.
          msg = fromString . LBS8.unpack $ Conf.mkMessage cfg

      hspec . with app' $ do

        -- AddRq Spec
        describe "POST /topic/add" $ do

          it "Should return 200 with well formed request" $ do
            post "/fudge/add" "Fred" `shouldRespondWith` msg { matchStatus = 200 }

          it "Should 400 on empty input" $
            post "/fudge/add" "" `shouldRespondWith` 400

          it "Should add a comment to the topic"
            Test.Hspec.Wai.pending

        -- ViewRq Spec
        describe "GET /topic/view" $ do
          it "Should return 200 with content" $
            get "/fudge/view" `shouldRespondWith` "Susan was ere" { matchStatus = 200 }

        -- ListRq Spec
        describe "GET /list" $ do
          it "Should return 200 with content" $
            get "/list" `shouldRespondWith` "[ \"Fred wuz ere\", \"Susan was ere\" ]" { matchStatus = 200 }
