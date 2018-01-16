module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/FirstApp/Conf.hs"
  ]
