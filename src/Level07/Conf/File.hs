module Level07.Conf.File where

import           Data.Aeson                 (eitherDecode)

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Data.Text                  (pack)

import           Data.Bifunctor             (first)

import           Control.Exception          (try)

import           Level07.Types              (ConfigError (..), PartialConf)

-- Doctest setup section
-- $setup
-- >>> :set -XOverloadedStrings

-- | Update these tests when you've completed this function.
--
-- | readConfFile
-- >>> readConfFile "badFileName.no"
-- Left (ConfigFileReadError badFileName.no: openBinaryFile: does not exist (No such file or directory))
-- >>> readConfFile "files/test.json"
-- Right "{\"foo\":33}\n"
--
readConfFile
  :: FilePath
  -> IO ( Either ConfigError ByteString )
readConfFile fp =
  first ConfigFileReadError <$> try (LBS.readFile fp)

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> IO ( Either ConfigError PartialConf )
parseJSONConfigFile fp =
  (first BadConfFile . eitherDecode =<<) <$> readConfFile fp
