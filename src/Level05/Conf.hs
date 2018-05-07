{-# LANGUAGE OverloadedStrings #-}
module Level05.Conf
    ( Conf (..)
    , firstAppConfig
    ) where

data Conf = Conf
  { dbFilePath :: FilePath
  }

firstAppConfig :: Conf
firstAppConfig = Conf "app_db.db"
