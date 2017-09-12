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
  | JSONDecodeError String
  | JSONFileReadError IOError
  -- Do we have anything else that can go wrong with our Conf now ?
  deriving Show

newtype Port = Port
  { getPort :: Word16 }
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

confPortToWai
  :: Conf
  -> Int
confPortToWai =
  fromIntegral . getPort . port

data PartialConf = PartialConf
  { pcPort     :: Last Port
  , pcHelloMsg :: Last HelloMsg
  }

-- Note that the types won't be able to completely save you here, if you mess up
-- the ordering of your 'a' and 'b' you will not end up with the desired result.
instance Monoid PartialConf where
  mempty = PartialConf mempty mempty

  mappend a b = PartialConf
    -- Compiler tells us about the little things we might have forgotten.
    { pcPort      = pcPort a <> pcPort b
    , pcHelloMsg  = pcHelloMsg a <> pcHelloMsg b
    }

-- We have some sane defaults that we can always rely on, so define them using
-- our PartialConf.
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
      (maybe (Left e) Right . getLast . g) pc

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
    toPartialConf
      :: Aeson.Object
      -> PartialConf
    toPartialConf cObj = PartialConf
      ( fromJsonObjWithKey "port" Port cObj )
      ( fromJsonObjWithKey "helloMsg" helloFromStr cObj )

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
  <*> (Last <$> optional (helloFromStr <$> strOption helloMods))
  -- Add our two new fields to the parsing of the PartialConf record. Note that
  -- if you update the data structure the compiler will do its best to inform
  -- you about everywhere that needs attention.
  where
    helloMods = long "hello-msg"
                <> short 'm'
                <> metavar "HELLOMSG"
                <> help "Message to respond to requests with."

    -- dbFilePathMods = long "db-filepath"
    --              <> short 'd'
    --              <> metavar "DBFILEPATH"
    --              <> help "FilePath to the SQLite DB"

    -- tableMods = long "table-name"
    --              <> short 't'
    --              <> metavar "TABLENAME"
    --              <> help "Comments DB table name"

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
