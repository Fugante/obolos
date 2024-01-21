{-# LANGUAGE DeriveGeneric #-}

module DB.Entities.Category
    ( Category(..)
    , catToTuple
    , tupleToCat
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
import Database.HDBC (toSql, SqlValue (SqlInteger, SqlByteString))
import Data.ByteString.Char8 (unpack)


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
catToTuple (Category i c s) = [toSql i, toSql c, toSql s]

tupleToCat :: Tuple -> Category
tupleToCat [SqlInteger i, SqlByteString c, SqlInteger s] =
    Category (Just i) (Just $ unpack c) (Just s)
tupleToCat _ =
    Category Nothing Nothing Nothing