{-# LANGUAGE OverloadedStrings #-}
module Level05Tests
  ( unitTests
  , doctests
  ) where

import           Control.Monad.Reader (ask, reader)

import           Data.Monoid          ((<>))

import           Data.String          (IsString)

import           Test.Hspec
import           Test.Hspec.Wai

import qualified System.Exit          as Exit

import qualified Level05.AppM         as AppM

import qualified Level05.Core         as Core
import qualified Level05.DB           as DB
import qualified Level05.Types        as Types

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

    Right db -> do
      let app' = pure (Core.app db)

          flushTopic =
            -- Clean up and yell about our errors
            either dieWith pure =<< AppM.runAppM (
            -- We don't export the constructor so even for known values we have
            -- to play by the rules. There is no - "Oh just this one time.", do it right.
            AppM.liftEither (Types.mkTopic testTopic)
              -- Purge all of the comments for this topic for our tests
              >>= DB.deleteTopic db
            )

      -- Run the tests with a DB topic flush between each spec
      hspec . with ( flushTopic >> app' ) $ do
        -- Save us a bit of repetition
        let pOST = post ( "/" <> testTopic <> "/add" )

        -- AddRq Spec
        describe "POST /topic/add" $ do
          it "Should return 200 with well formed request" $
            pOST "Is super tasty." `shouldRespondWith` "Success"

          it "Should 400 on empty input" $
            pOST "" `shouldRespondWith` 400

        -- ViewRq Spec
        describe "GET /topic/view" $
          it "Should return 200 with content" $ do
            _ <- pOST "Is super tasty."
            get ( "/" <> testTopic <> "/view" ) `shouldRespondWith` 200

        -- ListRq Spec
        describe "GET /list" $
          it "Should return 200 with content" $ do
            _ <- pOST "Is super tasty."
            get "/list" `shouldRespondWith` "[\"fudge\"]"
