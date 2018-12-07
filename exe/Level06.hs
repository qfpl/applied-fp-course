module Main where

import qualified Level06.Core as Core

-- Our application will be built as a library that will be included in an
-- executable. So our ``exe/Main.hs`` is a straightforward and unremarkable
-- affair.
main :: IO ()
main = Core.runApplication
