{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Test.Hspec
import           Test.Hspec.Wai

import qualified System.Exit                as Exit

import qualified Data.ByteString.Lazy.Char8 as LBS8

import qualified FirstApp.Conf              as Conf
import qualified FirstApp.Main              as Main

main :: IO ()
main = do
  -- We're still going to be running our application so we need to load the config.
  cfgE <- Conf.parseOptions "appconfig.json"
  case cfgE of

    Left err ->
      -- Die loudly if the config couldn't be loaded. Ensure we exit with a
      -- failure code to trip up anything that might be watching our run status.
      error "Config failure error handling not implemented"

    Right cfg -> do
      -- We need to setup our Application.
      let app' = undefined

      -- What function in the Hspec or Hspec.Wai package do we need to use here
      -- so that Hspec can manage the running of our Wai Application?
      hspec . with app' $ do
          -- Here is an example test for the 'ListRq' route.
          -- Start with a general description of what we're going to test.
          describe "List Route" $ do
            -- Individual test cases provide more precise information regarding
            -- what they are going to test.
            it "Should return a 'not implemented' message and 200 status" $
              -- Using the functions from ``Test.Hspec.Wai`` this actions a GET request
              -- on the "/list" route, and using an infix function, compares the result of
              -- that request to our expected result.

              -- There String literal here is being converted by the use of the
              -- ``IsString`` typeclass into a response type that Hspec.Wai can
              -- use. Check the documentation for more examples, but when given
              -- a string literal, it will assume that is the expected body of
              -- the request and also check for a 200 response code.
              get "/list" `shouldRespondWith` "List Request not implemented"
