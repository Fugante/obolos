{-# LANGUAGE DeriveGeneric #-}

module DB.Entities.Category
    ( Category(..)
    , catToTuple
    )
    where

import GHC.Generics (Generic)
import Data.Aeson
    ( FromJSON
    , ToJSON (toEncoding)
    , genericToEncoding
    , defaultOptions
    )

import DB.Relations (Tuple)
import Database.HDBC (toSql)


data Category =
    Category
    { id :: Maybe Integer
    , category :: Maybe String
    , supercategory :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance FromJSON Category

instance ToJSON Category where
    toEncoding = genericToEncoding defaultOptions


catToTuple :: Category -> Tuple
catToTuple (Category i c sc) = [toSql i, toSql c, toSql sc]