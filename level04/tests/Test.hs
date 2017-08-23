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
  -- We're still going to be running our application so we need to load the config.
  cfgE <- Conf.parseOptions "appconfig.json"
  case cfgE of

    Left err ->
      error "Config failure error handling not implemented"
      -- do
      -- -- Die loudly if the config couldn't be loaded, ensuring we exit with a
      -- -- failure code to trip up anything that might be watching out run status.
      -- print err
      -- Exit.exitFailure

    Right cfg ->
      error "tests not implemented"
      -- do
      -- let app' = pure ( Main.app cfg )
      --     -- The hspec-wai package uses the IsString typeclass to be able to turn
      --     -- String values into a ResponseMatcher record type. It's not terribly
      --     -- transparent or elegant, but it works sufficiently well for these tests.
      --     msg = fromString . LBS8.unpack $ Conf.mkMessage cfg

      -- -- The hspec package provides a runner that is a contained context where
      -- -- all of the specifications are executed.
      -- hspec . with app' $ do
      --   -- For each specification, you describe and initial aspect and then
      --   -- populate it with all of the individual test cases.

      --   -- AddRq Spec
      --   describe "POST /topic/add" $ do

      --     it "Should return 200 with well formed request" $ do
      --       post "/fudge/add" "Fred" `shouldRespondWith` msg

      --     it "Should 400 on empty input" $
      --       post "/fudge/add" "" `shouldRespondWith` 400

      --     it "Should add a comment to the topic" $
      --       Test.Hspec.Wai.pendingWith "No db attached, yet..."

      --   -- ViewRq Spec
      --   describe "GET /topic/view" $ do
      --     it "Should return 200 with content" $
      --       get "/fudge/view" `shouldRespondWith` "Susan was ere"

      --   -- ListRq Spec
      --   describe "GET /list" $ do
      --     it "Should return 200 with content" $
      --       get "/list" `shouldRespondWith` "[ \"Fred wuz ere\", \"Susan was ere\" ]"
