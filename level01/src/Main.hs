module Main where

import qualified FirstApp.Main as Main

-- Our application will be built as a library that will be included in an
-- executable. So our ``bin/Main.hs`` is a straightforward and unremarkable
-- affair. We won't be updating this file.
main :: IO ()
main = Main.runApp
