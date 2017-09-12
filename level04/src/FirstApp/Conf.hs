{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Conf
    ( Conf (..)
    , Port (getPort)
    , HelloMsg (getHelloMsg)
    , parseOptions
    , mkMessage
    , confPortToWai
    ) where

import           Control.Exception          (catch)

import           GHC.Word                   (Word16)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last, getLast),
                                             Monoid (mappend, mempty), (<>))
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

-- Doctest setup section
-- $setup
-- >>> :set -XOverloadedStrings

-- Similar to when we were considering what might go wrong with the RqTypes, lets
-- think about might go wrong when trying to gather our configuration information.
data ConfigError
  = MissingPort
  | MissingHelloMsg
  | JSONFileReadError IOError
  | JSONDecodeError String
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
  Last ( c <$> Aeson.parseMaybe (Aeson..: k) obj )

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
