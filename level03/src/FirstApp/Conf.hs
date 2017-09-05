{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module FirstApp.Conf
    ( Conf (..)
    , Port (getPort)
    , HelloMsg (getHelloMsg)
    , parseOptions
    ) where

import           Control.Exception          (bracketOnError)

import           Data.Maybe                 (fromMaybe)
import           Data.Monoid                (Last (..), Monoid (..), (<>))
import           Data.String                (fromString)

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Text                  (Text)

import           Data.Aeson                 (FromJSON)

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as Aeson

import           Options.Applicative        (Parser, ParserInfo, eitherReader,
                                             execParser, fullDesc, header, help,
                                             helper, info, long, metavar,
                                             option, optional, progDesc, short,
                                             strOption)

import           Text.Read                  (readEither)

newtype Port = Port
  { getPort :: Int }
  deriving (Eq, Show)

newtype HelloMsg = HelloMsg
  { getHelloMsg :: ByteString }
  deriving (Eq, Show)

-- The ``Conf`` type will need:
-- - A customisable port number: `Port`
-- - A changeable message for our users: `HelloMsg`
data Conf = Conf

-- Similar to when we were considering what might go wrong with the RqType, lets
-- think about might go wrong when trying to gather our config information.
data ConfigError

-- Our application will be able to load configuration from both a file and
-- command line input. We want to be able to use the command line to temporarily
-- override the configuration from our file. How to would you combine them? This
-- question will help us find which abstraction is correct for our needs...

-- We want the CommandLine configuration to take precedence over the File
-- configuration, so if we think about combining each of our ``Conf`` records,
-- we want to be able to write something like this:

-- defaults <> file <> commandLine

-- We can use the ``Monoid`` typeclass to handle combining the ``Conf`` records
-- together, and the Last newtype to wrap up our values to handle the desired
-- precedence. The Last newtype is a wrapper for Maybe that when used with its
-- ``Monoid`` instance will always preference the last Just value that it has:

-- Last (Just 3) <> Last (Just 1) = Last (Just 1)
-- Last Nothing  <> Last (Just 1) = Last (Just 1)
-- Last (Just 1) <> Last Nothing  = Last (Just 1)

-- To make this easier, we'll make a new type `PartialConf` that will have our
-- Last wrapped values. We can then define a ``Monoid`` instance for it and have
-- our ``Conf`` be a known good configuration.
data PartialConf

-- We now define our ``Monoid`` instance for PartialConf. Allowing us to define
-- our always empty configuration, which would always fail our requirements.
-- More interestingly, we define our ``mappend`` function to lean on the
-- ``Monoid`` instance for Last to always get the last value.
instance Monoid PartialConf where

-- Set some sane defaults that we can always rely on
defaultConf
  :: PartialConf
defaultConf =
  error "defaultConf not implemented"

-- We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig =
  error "makeConfig not implemented"

-- This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
parseOptions
  :: FilePath
  -> IO (Either ConfigError Conf)
parseOptions =
  error "parseOptions not implemented"

-- | File Parsing

-- We're trying to avoid complications when selecting a configuration file
-- package from Hackage. We'll use an encoding that you are probably familiar
-- with, for better or worse, and write a small parser to pull out the bits we
-- need. The package we're using is the ``aeson`` package to parse some JSON and
-- we'll pick the bits off the Object.

-- The documentation for this package will guide you in the right direction
parseJSONConfigFile
  :: FilePath
  -> IO PartialConf
parseJSONConfigFile =
  error "parseJSONConfigFile not implemented"

-- | Command Line Parsing

-- We will use the ``optparse-applicative`` package to build our command line
-- parser. As this particular problem is fraught with silly dangers and we
-- appreciate someone else having eaten this gremlin on our behalf.

-- You'll need to use the documentation for ``optparse-applicative`` to help you
-- write these functions as we're relying on their API to produce the types we
-- need. We've provided some of the less interesting boilerplate for you.
commandLineParser
  :: ParserInfo PartialConf
commandLineParser =
  let mods = fullDesc
        <> progDesc "Manage comments for something"
        <> header "Your first Haskell app!"
  in
    info (helper <*> partialConfParser) mods

-- Combine the smaller parsers into our larger PartialConf type.
partialConfParser
  :: Parser PartialConf
partialConfParser =
  error "partialConfParser not implemented"

-- Parse the Port value off the command line args and into a Last wrapper.
portParser
  :: Parser (Last Port)
portParser =
  let
    -- mods = long "port"
    --        <> short 'p'
    --        <> metavar "PORT"
    --        <> help "TCP Port to accept requests on"
  in
    error "portParser not implemented"

-- Parse the HelloMsg from the input string into our type and into a Last wrapper.
helloMsgParser
  :: Parser (Last HelloMsg)
helloMsgParser =
  let
    -- mods = long "hello-msg"
    --        <> short 'm'
    --        <> metavar "HELLOMSG"
    --        <> help "Message to respond to requests with."
  in
    error "helloMsgParser not implemented"
