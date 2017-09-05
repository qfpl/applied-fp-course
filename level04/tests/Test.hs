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
      let app' = undefined
      hspec . undefined $ do
          describe undefined $ do
            error "test undefined"
