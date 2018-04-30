module Main where

import qualified Level06.Main as Main

-- Our application will be built as a library that will be included in an
-- executable. So our ``exe/Main.hs`` is a straightforward and unremarkable
-- affair.
main :: IO ()
main = Main.runApp
