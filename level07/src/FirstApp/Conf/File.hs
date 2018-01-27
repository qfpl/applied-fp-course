module FirstApp.Conf.File where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Data.Bifunctor             (first)

import           Control.Exception          (try)

import qualified Data.Aeson                 as Aeson

import           FirstApp.Types             (ConfigError (..), PartialConf)

-- Doctest setup section
-- $setup
-- >>> :set -XOverloadedStrings

-- | Update these tests when you've completed this function.
--
-- | readConfFile
-- >>> readConfFile "badFileName.no"
-- Left (ConfigFileReadError badFileName.no: openBinaryFile: does not exist (No such file or directory))
-- >>> readConfFile "test.json"
-- Right "{\"foo\":33}\n"
--
readConfFile
  :: FilePath
  -> IO ( Either ConfigError ByteString )
readConfFile fp =
  first ConfigFileReadError <$> try (LBS.readFile fp)

-- Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile
  :: FilePath
  -> IO ( Either ConfigError PartialConf )
parseJSONConfigFile fp =
  (first JSONDecodeError . Aeson.eitherDecode =<<) <$> readConfFile fp

