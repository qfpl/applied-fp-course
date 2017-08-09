{-# LANGUAGE OverloadedStrings #-}
module Conf
    ( Conf (..)
    , Port (getPort)
    , HelloMsg (getHelloMsg)
    , parseOptions
    ) where

import Control.Exception (bracketOnError)

import Data.Monoid (Monoid (..), Last (..), (<>))
import Data.String (fromString)
import Data.Maybe (fromMaybe) 

import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Aeson (FromJSON)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson

import Options.Applicative (Parser, ParserInfo, eitherReader,
                            execParser, fullDesc, header, help,
                            helper, info, long, metavar, option,
                            optional, progDesc, short, strOption)

import Text.Read (readEither)

import qualified Data.Yaml.Config as Y

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

data Conf = Conf
  { port     :: Port
  , helloMsg :: HelloMsg
  }

data PartialConf = PartialConf
  { pcPort     :: Last Port
  , pcHelloMsg :: Last HelloMsg
  }

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
    lastToEither e g =
      maybe (Left e) Right . getLast $ g pc

parseOptions
  :: FilePath
  -> IO (Either ConfigError Conf)
parseOptions fp = do
  fileConf <- parseYAMLConfigFile fp
  cmdLine <- execParser commandLineParser
  pure $ makeConfig (defaultConf <> fileConf <> cmdLine)

-- | File Parsing

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

    offObj
      :: FromJSON a
      => Text
      -> (a -> b)
      -> Aeson.Object
      -> Last b
    offObj k c obj =
      Last $ c <$> Aeson.parseMaybe (Aeson..: k) obj

    readObject
      :: IO (Maybe Aeson.Object)
    readObject = bracketOnError
      (LBS.readFile fp)
      (\_ -> pure Nothing)
      (pure . Aeson.decode)

parseYAMLConfigFile
  :: FilePath
  -> IO PartialConf
parseYAMLConfigFile fp =
  let fileConfSucc cfgF = do
        let port'     = getCfg Port "port" cfgF
            helloMsg' = getCfg helloFromStr "port" cfgF

        pure $ PartialConf port' helloMsg'
  in
    bracketOnError (Y.load fp) (const emptyConf) fileConfSucc
  where
    emptyConf :: IO PartialConf
    emptyConf = pure mempty
 
    getCfg f k c =
      Last $ f <$> Y.lookupDefault k Nothing c

-- | Command Line Parsing

commandLineParser
  :: ParserInfo PartialConf
commandLineParser =
  let mods = fullDesc
        <> progDesc "Manage comments for something"
        <> header "Your first Haskell app!"
  in
    info (helper <*> partialConfParser) mods

partialConfParser
  :: Parser PartialConf
partialConfParser = PartialConf
  <$> portParser
  <*> helloMsgParser

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

helloMsgParser
  :: Parser (Last HelloMsg)
helloMsgParser =
  let mods = long "hello-msg"
             <> short 'm'
             <> metavar "HELLOMSG"
             <> help "Message to respond to requests with."
  in
    Last <$> optional (helloFromStr <$> strOption mods)
