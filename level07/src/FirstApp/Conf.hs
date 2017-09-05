{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Conf
    ( Conf (..)
    , ConfigError (..)
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
import qualified Data.Text                  as Text

import           Data.Aeson                 (FromJSON)

import qualified Data.Aeson                 as Aeson
import qualified Data.Aeson.Types           as Aeson

import           Options.Applicative        (Parser, ParserInfo, eitherReader,
                                             execParser, fullDesc, header, help,
                                             helper, info, long, metavar,
                                             option, optional, progDesc, short,
                                             strOption)

import           Text.Read                  (readEither)

import           FirstApp.DB.Types                (Table (..))

data ConfigError
  = MissingPort
  | MissingHelloMsg
  | MissingTableName
  | MissingDbFilePath
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
  { port       :: Port
  , helloMsg   :: HelloMsg
  , tableName  :: Table
  , dbFilePath :: FilePath
  }

data PartialConf = PartialConf
  { pcPort       :: Last Port
  , pcHelloMsg   :: Last HelloMsg
  , pcTableName  :: Last Table
  , pcDbFilePath :: Last FilePath
  }

instance Monoid PartialConf where
  mempty = PartialConf mempty mempty mempty mempty

  mappend a b = PartialConf
    -- Compiler tells us about the little things we might have forgotten.
    { pcPort      = pcPort a <> pcPort b
    , pcHelloMsg  = pcHelloMsg a <> pcHelloMsg b
    , pcTableName = pcTableName a <> pcTableName b
    , pcDbFilePath = pcDbFilePath a <> pcDbFilePath b
    }

-- We have some sane defaults that we can always rely on, so define them using
-- our PartialConf.
defaultConf
  :: PartialConf
defaultConf = PartialConf
  (pure (Port 3000))
  (pure (HelloMsg "World!"))
  (pure (Table "comments"))
  (pure "firstapp_db.db")

-- We need something that will take our PartialConf and see if can finally build
-- a complete Conf record. Also we need to highlight any missing config values
-- by providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig pc = Conf
  <$> lastToEither MissingPort pcPort
  <*> lastToEither MissingHelloMsg pcHelloMsg
  <*> lastToEither MissingTableName pcTableName
  <*> lastToEither MissingDbFilePath pcDbFilePath
  where
    -- You don't need to provide type signatures for most functions in where/let
    -- sections. Sometimes the compiler might need a bit of help, or you would
    -- like to be explicit in your intentions.
    lastToEither e g =
      (maybe (Left e) Right . getLast . g) pc

-- This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.
parseOptions
  :: FilePath
  -> IO (Either ConfigError Conf)
parseOptions fp = do
  fileConf <- parseJSONConfigFile fp
  cmdLine  <- execParser commandLineParser
  pure $ makeConfig (defaultConf <> fileConf <> cmdLine)

-- | File Parsing

-- | fromJsonObjWithKey
-- >>> fromJsonObjWithKey "foo" id (encode "{\"foo\":\"Susan\"}")
-- Last (Just "Susan")
-- >>> fromJsonObjWithKey "foo" id (encode "{\"bar\":33}")
-- Last Nothing
fromJsonObjWithKey
  :: FromJSON a
  => Text
  -> (a -> b)
  -> Aeson.Object
  -> Last b
fromJsonObjWithKey k c obj =
  Last (c <$> Aeson.parseMaybe (Aeson..: k) obj)

parseJSONConfigFile
  :: FilePath
  -> IO PartialConf
parseJSONConfigFile fp = do
  fc <- readObject
  pure . fromMaybe mempty $ toPartialConf <$> fc
  where
    toPartialConf cObj = PartialConf
      ( fromJsonObjWithKey "port" Port cObj )
      ( fromJsonObjWithKey "helloMsg" helloFromStr cObj )
      -- Pull the extra keys off the configuration file.
      ( fromJsonObjWithKey "tableName" Table cObj )
      ( fromJsonObjWithKey "dbFilePath" id cObj )

    readObject
      :: IO (Maybe Aeson.Object)
    readObject = bracketOnError
      (LBS.readFile fp)
      (pure . const Nothing)
      (pure . Aeson.decode)

-- | Command Line Parsing

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
  <*> strParse helloFromStr helloMods
  -- Add our two new fields to the parsing of the PartialConf record. Note that
  -- if you update the data structure the compiler will do its best to inform
  -- you about everywhere that needs attention.
  <*> strParse ( Table . Text.pack ) tableMods
  <*> strParse id dbFilePathMods
  where
    -- With the addition of two new very similar parsers, we can abstract out
    -- part of the construction into a separate function so we avoid repeating
    -- ourselves.
    strParse c m =
      Last <$> optional (c <$> strOption m)

    helloMods = long "hello-msg"
                <> short 'm'
                <> metavar "HELLOMSG"
                <> help "Message to respond to requests with."

    dbFilePathMods = long "db-filepath"
                 <> short 'd'
                 <> metavar "DBFILEPATH"
                 <> help "FilePath to the SQLite DB"

    tableMods = long "table-name"
                 <> short 't'
                 <> metavar "TABLENAME"
                 <> help "Comments DB table name"

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
