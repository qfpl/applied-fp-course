{-# LANGUAGE OverloadedStrings #-}
module Level06Tests
  ( doctests
  , unitTests
  ) where

import           Control.Monad  (join)

import           Data.Semigroup ((<>))
import           Data.String    (IsString)

import           Test.Hspec
import           Test.Hspec.Wai

import qualified System.Exit    as Exit

import qualified Level06.AppM   as AppM
import qualified Level06.Core   as Core
import qualified Level06.DB     as DB
import qualified Level06.Types  as Types

doctests :: [FilePath]
doctests =
  [ "-isrc"
  , "src/Level06/Conf.hs"
  , "src/Level06/DB.hs"
  , "src/Level06/Types.hs"
  ]

unitTests :: IO ()
unitTests = do
  let
    dieWith :: Show a => a -> IO ()
    dieWith m = print m >> Exit.exitFailure

    testTopic :: IsString s => s
    testTopic = "fudge"

  reqsE <- Core.prepareAppReqs
  case reqsE of

    Left err -> dieWith err

    Right ( cfg, db ) -> do
      let app' = pure (Core.app cfg db)

          flushTopic :: IO ()
          flushTopic = either dieWith pure =<< AppM.runAppM
            (AppM.liftEither (Types.mkTopic testTopic) >>= DB.deleteTopic db)

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
