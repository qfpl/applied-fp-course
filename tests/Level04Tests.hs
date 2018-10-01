{-# LANGUAGE OverloadedStrings #-}
module Level04Tests
  ( unitTests
  , doctests
  ) where

import           Control.Monad  (join)
import Control.Monad.IO.Class (liftIO)

import qualified System.Exit    as Exit

import Data.Foldable (traverse_)
import Data.Semigroup ((<>))

import           Network.HTTP.Types as HTTP

import           Helpers            (TestM, assertBody, assertStatus, get, post, runTestsFor)

import qualified Level04.Core   as Core
import qualified Level04.DB     as DB
import qualified Level04.Types  as Types

-- Don't forget to uncomment these functions in @tests/Test.hs@ otherwise your
-- tests won't be run.

doctests :: [FilePath]
doctests =
  [ "-isrc"
  , "src/Level04/Conf.hs"
  , "src/Level04/DB.hs"
  , "src/Level04/Types.hs"
  ]

dieWith :: Show a => a -> IO ()
dieWith err = print err >> Exit.exitFailure

unitTests :: IO ()
unitTests = do
  reqE <- Core.prepareAppReqs
  case reqE of
    Left err -> dieWith err
    Right db -> runTestsFor (Core.app db) "Level 04 Tests" $ do

      let
        flushTopic :: TestM ()
        flushTopic = liftIO .
          -- Clean up and yell about our errors
          (traverse_ (either dieWith pure) =<<) .
          -- Purge all of the comments for this topic for our tests
          traverse ( DB.deleteTopic db )
          -- We don't export the constructor so even for known values we have
          -- to play by the rules. There is no - "Oh just this one time.", do it right.
          $ Types.mkTopic "fudge"

        -- Run a test and then flush the db
        test t = t >> flushTopic

        topicR = "/fudge/"

        addToTopic =
          post "Add Topic" (topicR <> "add") "Fred"

      -- AddRq Spec
      -- it should return 200 with well formed request
      test $ addToTopic >>= assertBody "Success"

      -- it should 400 on empty input
      test $ post "Empty Input" (topicR <> "add") ""
        >>= assertStatus HTTP.status400

      -- ViewRq Spec
      -- it should return 200 with
      test $ addToTopic
        >> get "View topic" (topicR <> "view")
        >>= assertStatus HTTP.status200

      -- ListRq Spec
      test $ addToTopic
        >> get "List topics" "/list"
        >>= assertBody "[\"fudge\"]"
