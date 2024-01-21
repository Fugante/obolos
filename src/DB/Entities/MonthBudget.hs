{-# LANGUAGE DeriveGeneric #-}

module DB.Entities.MonthBudget
    ( MonthBudget(..)
    , mbToTuple
    , tupleToMb
    )
    where

-- Dependencies
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
import Database.HDBC


data MonthBudget =
    MonthBudget
    { id :: Maybe Integer
    , year :: Maybe Integer
    , month :: Maybe Integer
    , categoryId :: Maybe Integer
    , planned :: Maybe Integer
    , actual :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance FromJSON MonthBudget

instance ToJSON MonthBudget where
    toEncoding = genericToEncoding defaultOptions


mbToTuple :: MonthBudget -> Tuple
mbToTuple (MonthBudget i y m c p a) =
    [toSql i, toSql y, toSql m, toSql c, toSql p, toSql a]

tupleToMb :: Tuple -> MonthBudget
tupleToMb
    [SqlInteger i, SqlInteger y, SqlInt32 m, SqlInteger c, SqlInteger p, SqlInteger a] =
        MonthBudget (Just i) (Just y) (Just $ toInteger m) (Just c) (Just p) (Just a)
tupleToMb _ =
    MonthBudget Nothing Nothing Nothing Nothing Nothing Nothing