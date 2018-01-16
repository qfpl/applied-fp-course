{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module FirstApp.Conf
    ( parseOptions
    ) where

import           GHC.Word                  (Word16)

import           Data.Bifunctor            (first)
import           Data.Monoid               (Last (..), (<>))

import           FirstApp.Types            (Conf (..), ConfigError (..),
                                            DBFilePath (DBFilePath),
                                            PartialConf (..), Port (Port))

import           FirstApp.Conf.CommandLine (commandLineParser)
import           FirstApp.Conf.File        (parseJSONConfigFile)

-- We have some sane defaults that we can always rely on, so define them using
-- our PartialConf.
defaultConf
  :: PartialConf
defaultConf = PartialConf
  (pure (Port 3000))
  (pure (DBFilePath "app_db.db"))

-- We need something that will take our PartialConf and see if can finally build
-- a complete Conf record. Also we need to highlight any missing config values
-- by providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig pc = Conf
  <$> lastToEither MissingPort pcPort
  <*> lastToEither MissingDBFilePath pcDBFilePath
  where
    -- You don't need to provide type signatures for most functions in where/let
    -- sections. Sometimes the compiler might need a bit of help, or you would
    -- like to be explicit in your intentions.
    lastToEither
      :: ConfigError
      -> (PartialConf -> Last b)
      -> Either ConfigError b
    lastToEither e g =
      (maybe (Left e) Right . getLast . g) pc

-- This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
parseOptions
  :: FilePath
  -> IO (Either ConfigError Conf)
parseOptions fp =
  let mkCfg cli file = makeConfig (defaultConf <> file <> cli)
  in do
    cli' <- commandLineParser
    ( >>= mkCfg cli' ) <$> parseJSONConfigFile fp
