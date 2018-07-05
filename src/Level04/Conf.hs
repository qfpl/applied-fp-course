{-# LANGUAGE OverloadedStrings #-}
module Level04.Conf
    ( Conf (..)
    , firstAppConfig
    ) where

-- We'll do more with this later, but we can easily stub it to keep things
-- rolling and come back to refactor it later.
data Conf = Conf
  { dbFilePath :: FilePath
  }

-- A simple default that we could just inline this right where we need it. But
-- types are so cheap that we can easily prepare ourselves for "doing the right
-- thing".
firstAppConfig :: Conf
firstAppConfig = Conf "app_db.db"
