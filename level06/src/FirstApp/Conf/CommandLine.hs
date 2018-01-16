module FirstApp.Conf.CommandLine
  ( commandLineParser
  ) where

import           Data.Monoid         (Last (Last), (<>))

import           Options.Applicative (Parser, eitherReader, execParser,
                                      fullDesc, header, help, helper, info,
                                      long, metavar, option, optional, progDesc,
                                      short, strOption)

import           Text.Read           (readEither)

import           FirstApp.Types      (DBFilePath (DBFilePath),
                                      PartialConf (PartialConf), Port (Port))

-- | Command Line Parsing

-- This is an example of using the ``optparse-applicative`` package to build our command line
-- parser. As this particular problem is fraught with silly dangers and we appreciate someone else
-- having eaten this gremlin on our behalf.
commandLineParser
  :: IO PartialConf
commandLineParser =
  let mods = fullDesc
        <> progDesc "Manage comments for something"
        <> header "Your first Haskell app!"
  in
    execParser $ info (helper <*> partialConfParser) mods

-- Combine the smaller parsers into our larger ``PartialConf`` type.
partialConfParser
  :: Parser PartialConf
partialConfParser =
  PartialConf <$> portParser <*> dbFilePathParser

-- Parse the Port value off the command line args and into a Last wrapper.
portParser
  :: Parser (Last Port)
portParser =
  let
    mods = long "port"
           <> short 'p'
           <> metavar "PORT"
           <> help "TCP Port to accept requests on"
    -- A custom parser to turn a String into a Word16, before putting it into a Port
    portReader = eitherReader (fmap Port . readEither)
  in
    Last <$> optional (option portReader mods)

-- Parse the DBFilePath from the input string into our type and into a Last wrapper.
dbFilePathParser
  :: Parser (Last DBFilePath)
dbFilePathParser =
  let
    mods = long "db-filepath"
           <> short 'd'
           <> metavar "DBFILEPATH"
           <> help "File path for our SQLite Database file."
  in
    Last <$> optional (DBFilePath <$> strOption mods)
