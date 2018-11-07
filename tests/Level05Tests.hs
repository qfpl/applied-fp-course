{-# LANGUAGE OverloadedStrings #-}
module Level05Tests
  ( unitTests
  , doctests
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ask, reader)

import           Data.Foldable          (traverse_)
import           Data.Monoid            ((<>))

import           Data.String            (IsString)

import           Network.HTTP.Types     as HTTP

import           Test.Hspec

import           Helpers                (TestM, assertBody, assertStatus, get,
                                         post, runTestsFor)

import qualified System.Exit            as Exit

import qualified Level05.AppM           as AppM

import qualified Level05.Core           as Core
import qualified Level05.DB             as DB
import qualified Level05.Types          as Types

doctests :: [FilePath]
doctests =
  [ "-isrc"
  , "src/Level05/Conf.hs"
  , "src/Level05/DB.hs"
  , "src/Level05/Types.hs"
  ]

unitTests :: IO ()
unitTests = do
  let
    dieWith :: Show a => a -> IO ()
    dieWith m = print m >> Exit.exitFailure

    -- This helps keep the string polymorphic so we can use it in both
    -- ByteString and Text forms in this file, without having to run encoding
    -- functions. The compiler takes care of it for us.
    testTopic :: IsString s => s
    testTopic = "fudge"

  reqsE <- Core.prepareAppReqs
  case reqsE of
    Left err -> dieWith err
    Right db -> runTestsFor (Core.app db) "Level 05 Tests" $ do

      let
        flushTopic :: TestM ()
        flushTopic = liftIO .
          -- Clean up and yell about our errors
          (traverse_ (either dieWith pure) =<<) .
          -- Include the runner to handle our new 'AppM'
          AppM.runAppM .
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
