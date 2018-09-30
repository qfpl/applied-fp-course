module Main where

import qualified Level03Tests
import qualified Level04Tests
import qualified Level05Tests
import qualified Level06Tests
import qualified Level07Tests

main :: IO ()
main = do
  -- putStrLn "No tests yet!"
  Level03Tests.unitTests
  -- Level04Tests.unitTests
  -- Level05Tests.unitTests
  -- Level06Tests.unitTests
  -- Level07Tests.unitTests
