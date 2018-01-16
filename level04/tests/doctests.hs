module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/FirstApp/Conf.hs"
  , "src/FirstApp/DB.hs"
  , "src/FirstApp/Types.hs"
  ]
