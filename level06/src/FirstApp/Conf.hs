{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Conf
    ( Conf (..)
    , ConfigError (..)
    , Port (getPort)
    , HelloMsg (getHelloMsg)
    , parseOptions
    , confPortToWai
    ) where

import           Control.Exception          (catch)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (..), Monoid (..), (<>))
import           Data.String                (fromString)
import           GHC.Word                   (Word16)

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

import           FirstApp.DB                (Table (..))

-- Doctest setup section
-- $setup
-- >>> :set -XOverloadedStrings

data ConfigError
  = MissingPort
  | MissingHelloMsg
  | MissingTableName
  | JSONFileReadError IOError
  | JSONDecodeError String
  | MissingDbFilePath
  deriving Show

newtype Port = Port
  { getPort :: Word16 }
  deriving Show

newtype HelloMsg = HelloMsg
  { getHelloMsg :: ByteString }
  deriving Show

-- This is a helper function to take a string and turn it into our HelloMsg type.
helloFromStr
  :: String
  -> HelloMsg
helloFromStr =
  HelloMsg . fromString

data Conf = Conf
  { port       :: Port
  , helloMsg   :: HelloMsg
  , tableName  :: Table
  , dbFilePath :: FilePath
  }

confPortToWai
  :: Conf
  -> Int
confPortToWai =
  fromIntegral . getPort . port

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

-- We have some sane defaults that we can always rely on, so define them using our PartialConf.
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
parseOptions fp =
  let mkCfg cli file = makeConfig (defaultConf <> file <> cli)
  in do
    cli' <- execParser commandLineParser
    ( >>= mkCfg cli' ) <$> parseJSONConfigFile fp

-- | File Parsing

-- | fromJsonObjWithKey
-- >>> let (Just obj) = ( Aeson.decode "{\"foo\":\"Susan\"}" ) :: Maybe Aeson.Object
--
-- >>> fromJsonObjWithKey "foo" (id :: Text -> Text) obj
-- Last {getLast = Just "Susan"}
--
-- >>> fromJsonObjWithKey "foo" id obj
-- Last {getLast = Nothing}
--
fromJsonObjWithKey
  :: FromJSON a
  => Text
  -> (a -> b)
  -> Aeson.Object
  -> Last b
fromJsonObjWithKey k c obj =
  Last $ c <$> Aeson.parseMaybe (Aeson..: k) obj

-- | decodeObj
-- >>> decodeObj ""
-- Left (JSONDecodeError "Error in $: not enough input")
--
-- >>> decodeObj "{\"bar\":33}"
-- Right (fromList [("bar",Number 33.0)])
--
decodeObj
  :: ByteString
  -> Either ConfigError Aeson.Object
decodeObj =
  first JSONDecodeError . Aeson.eitherDecode

-- | readObject
-- >>> readObject "badFileName.no"
-- Left (JSONFileReadError badFileName.no: openBinaryFile: does not exist (No such file or directory))
--
-- >>> readObject "test.json"
-- Right "{\"foo\":33}\n"
--
readObject
  :: FilePath
  -> IO (Either ConfigError ByteString)
readObject fp =
  (Right <$> LBS.readFile fp) `catch` (pure . Left . JSONFileReadError)

parseJSONConfigFile
  :: FilePath
  -> IO ( Either ConfigError PartialConf )
parseJSONConfigFile fp =
  (>>= fmap toPartialConf . decodeObj) <$> readObject fp
  where
    toPartialConf cObj = PartialConf
      ( fromJsonObjWithKey "port" Port cObj )
      ( fromJsonObjWithKey "helloMsg" helloFromStr cObj )
      -- Pull the extra keys off the configuration file.
      ( fromJsonObjWithKey "tableName" Table cObj )
      ( fromJsonObjWithKey "dbFilePath" id cObj )

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
  <*> strParse ( Table . Text.pack ) tableMods
  <*> strParse id dbFilePathMods
  where
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
