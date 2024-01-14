{-# LANGUAGE DeriveGeneric #-}

module Settings
    ( DB(..)
    , Settings(..)
    , getSettings
    , connString
    ) where

import Data.Aeson
import GHC.Generics


data DB =
    DB
    { user :: String
    , password :: String
    , database :: String
    , host :: String
    , port :: Integer
    } deriving (Generic, Show)

instance ToJSON DB where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON DB


data Settings =
    Settings
    { db :: DB } deriving (Generic, Show)

instance ToJSON Settings where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Settings


getSettings :: IO (Maybe Settings)
getSettings = decodeFileStrict "/home/fugante/Projects/obolos/settings.json"

connString :: DB -> String
connString (DB usr psw d h prt) = unwords
    [ "user=" ++ usr
    , "password=" ++ psw
    , "dbname=" ++ d
    , "host=" ++ h
    , "port=" ++ show prt
    ]