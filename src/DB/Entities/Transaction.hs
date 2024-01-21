{-# LANGUAGE DeriveGeneric #-}

module DB.Entities.Transaction
    ( Transaction(..)
    , tranToTuple
    )
    where

-- Dependencies
import Data.Time (LocalTime)
import GHC.Generics (Generic)

-- External dependencies
import Data.Aeson
    ( FromJSON
    , ToJSON (toEncoding)
    , genericToEncoding
    , defaultOptions
    )

-- Local modules
import DB.Relations (Tuple)
import Database.HDBC (toSql, SqlValue (SqlString))


data Transaction =
    Transaction
    { id :: Maybe Integer
    , amount :: Maybe Integer
    , categoryId :: Maybe Integer
    , date :: Maybe LocalTime
    , notes :: Maybe String
    } deriving (Eq, Show, Generic)

instance FromJSON Transaction

instance ToJSON Transaction where
    toEncoding = genericToEncoding defaultOptions


tranToTuple :: Transaction -> Tuple
tranToTuple (Transaction i a c d n) = [toSql i, toSql a, toSql c, toSql d, n']
    where
        n' = case n of {Nothing -> SqlString ""; Just n'' -> toSql n''}
