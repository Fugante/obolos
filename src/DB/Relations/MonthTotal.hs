{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
module DB.Relations.MonthTotal
    ( Year
    , Month
    , MonthT(..)
    , getMonthTotals
    )
    where

-- Dependencies
import Data.ByteString.Char8 (unpack)
import GHC.Generics (Generic)

-- External dependencies
import Data.Aeson
    ( FromJSON
    , ToJSON (toEncoding)
    , genericToEncoding
    , defaultOptions
    )
import Database.HDBC

--  Local modules
import DB.Entities.Entity


type Year = Integer
type Month = Integer

data MonthT =
    MonthT
    { category :: Maybe String
    , planned :: Maybe Integer
    , actual :: Maybe Integer
    , total :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance FromJSON MonthT

instance ToJSON MonthT where
    toEncoding = genericToEncoding defaultOptions

instance ToTuple MonthT where
    toTuple :: MonthT -> Tuple
    toTuple (MonthT c p a t) = [toSql c, toSql p, toSql a, toSql t]

instance FromTuple MonthT where
    fromTuple :: Tuple -> MonthT
    fromTuple [SqlByteString c, SqlInteger p, SqlInteger a, SqlInteger t] =
        MonthT (Just $ unpack c) (Just p) (Just a) (Just t)
    fromTuple _ = MonthT Nothing Nothing Nothing Nothing

getMonthTotals :: Year -> Month -> IO [MonthT]
getMonthTotals y m = do
    conn <- db
    mTs <- quickQuery'
        conn
        (  "SELECT c.category, m.planned, m.actual, m.planned - m.actual AS diff "
        ++ "FROM monthbudget AS m "
        ++ "JOIN category AS c ON m.category_id = c.id "
        ++ "WHERE m.year = ? AND m.month = ?;"
        )
        [toSql y, toSql m]
    return $ fmap fromTuple mTs