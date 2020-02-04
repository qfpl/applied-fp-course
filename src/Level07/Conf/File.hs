module Level07.Conf.File where

import Control.Exception (try)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as LBS
import Data.Text (pack)
import Level07.Types
  ( ConfigError (..),
    PartialConf,
    partialConfDecoder,
  )
import Waargonaut.Attoparsec (pureDecodeAttoparsecByteString)

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
readConfFile ::
  FilePath ->
  IO (Either ConfigError ByteString)
readConfFile fp =
  first ConfigFileReadError <$> try (LBS.readFile fp)

-- | Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile ::
  FilePath ->
  IO (Either ConfigError PartialConf)
parseJSONConfigFile fp =
  (first BadConfFile . runDecode =<<) <$> readConfFile fp
  where
    runDecode = pureDecodeAttoparsecByteString partialConfDecoder
