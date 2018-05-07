{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Level01.Core (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           ResponseReceived, responseLBS)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (status200)

-- Our "application" will respond to ALL incoming requests with a 200
-- status code response and the message "Hello, World!"
--
-- The `Application` type from Wai is a type synonym for the following type:
--
-- :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
--
-- So the first argument to our function below is the incoming Request, which we
-- disregard using `_`. The next input is the callback function that signals
-- when our request is complete.
--
-- This callback function is to signal to the Wai framework and we've completed
-- our part and it's free to close any resources it had and send the response.
--
-- This exercise is about tracking down the right documentation to find out what
-- you need to use to build this function. The types can guide you to some
-- extent, but you will have to be able to use the library documentation to find
-- what you need.
--
-- We've used the non-synonym version of the `Application` type below.
app
  :: Request
  -> (Response -> IO ResponseReceived)
  -> IO ResponseReceived
app _ cb =
  error "Application not implemented"

-- We keep this main function here as it is useful to build your application as
-- a library. The reasoning behind this is that when you come to do your
-- testing, you'll be able to import the entire application as a library without
-- needing to worry about any initialisation code you've buried in your
-- executable Main.hs.
runApp :: IO ()
runApp = run undefined undefined
