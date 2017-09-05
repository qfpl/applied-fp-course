{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Conf
    ( Conf (..)
    , Port (getPort)
    , HelloMsg (getHelloMsg)
    , parseOptions
    , mkMessage
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

-- Similar to when we were considering what might go wrong with the RqTypes, lets
-- think about might go wrong when trying to gather our configuration information.
data ConfigError
  = MissingPort
  | MissingHelloMsg
  deriving Show

newtype Port = Port
  { getPort :: Int }
  deriving Show

newtype HelloMsg = HelloMsg
  { getHelloMsg :: ByteString }
  deriving Show

helloFromStr
  :: String
  -> HelloMsg
helloFromStr =
  HelloMsg . fromString

mkMessage
  :: Conf
  -> LBS.ByteString
mkMessage =
  mappend "App says: "
  . getHelloMsg
  . helloMsg

data Conf = Conf
  { port     :: Port
  , helloMsg :: HelloMsg
  }

data PartialConf = PartialConf
  { pcPort     :: Last Port
  , pcHelloMsg :: Last HelloMsg
  }

-- Note that the types won't be able to completely save you here, if you mess up
-- the ordering of your 'a' and 'b' you will not end up with the desired result.
instance Monoid PartialConf where
  mempty = PartialConf mempty mempty

  mappend a b = PartialConf
    { pcPort     = pcPort a <> pcPort b
    , pcHelloMsg = pcHelloMsg a <> pcHelloMsg b
    }

defaultConf
  :: PartialConf
defaultConf = PartialConf
  (pure (Port 3000))
  (pure (HelloMsg "World!"))

makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig pc = Conf
  <$> lastToEither MissingPort pcPort
  <*> lastToEither MissingHelloMsg pcHelloMsg
  where
    -- You don't need to provide type signatures for most functions in where/let
    -- sections. Sometimes the compiler might need a bit of help, or you would
    -- like to be explicit in your intentions.
    lastToEither e g =
      maybe (Left e) Right . getLast $ g pc

parseOptions
  :: FilePath
  -> IO (Either ConfigError Conf)
parseOptions fp = do
  fileConf <- parseJSONConfigFile fp
  cmdLine  <- execParser commandLineParser
  pure $ makeConfig (defaultConf <> fileConf <> cmdLine)

-- | File Parsing

-- Additional Exercise: Rewrite this without using Do notation
-- 'fmap' should be sufficient.
parseJSONConfigFile
  :: FilePath
  -> IO PartialConf
parseJSONConfigFile fp = do
  fc <- readObject
  pure . fromMaybe mempty $ toPartialConf <$> fc
  where
    toPartialConf cObj = PartialConf
      ( offObj "port" Port cObj )
      ( offObj "helloMsg" helloFromStr cObj )

    -- Parse out the keys from the object, maybe...
    offObj
      :: FromJSON a
      => Text
      -> (a -> b)
      -> Aeson.Object
      -> Last b
    offObj k c obj =
      -- Too weird ?
      Last $ c <$> Aeson.parseMaybe (Aeson..: k) obj

    -- Use bracket to save ourselves from horrible exceptions, which are
    -- horrible.
    --
    -- Better ways to do this ?
    readObject
      :: IO (Maybe Aeson.Object)
    readObject = bracketOnError
      (LBS.readFile fp)
      (pure . const Nothing)
      (pure . Aeson.decode)

-- | Command Line Parsing

-- We will use the ``optparse-applicative`` package to build our command line
-- parser, as this problem is fraught with silly dangers and we appreciate
-- someone else having eaten this gremlin on our behalf.
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
partialConfParser = PartialConf
  <$> portParser
  <*> helloMsgParser

-- Parse the Port value off the command line args and into our Last wrapper.
portParser
  :: Parser (Last Port)
portParser =
  let mods = long "port"
             <> short 'p'
             <> metavar "PORT"
             <> help "TCP Port to accept requests on"
      portReader =
        eitherReader (fmap Port . readEither)
  in
    Last <$> optional (option portReader mods)

-- Parse the HelloMsg from the input string into our type and into a Last wrapper.
helloMsgParser
  :: Parser (Last HelloMsg)
helloMsgParser =
  let mods = long "hello-msg"
             <> short 'm'
             <> metavar "HELLOMSG"
             <> help "Message to respond to requests with."
  in
    Last <$> optional (helloFromStr <$> strOption mods)
