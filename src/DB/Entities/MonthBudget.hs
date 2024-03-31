{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module DB.Entities.MonthBudget
    ( MonthBudget(..)
    ) where

-- Dependencies
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


data MonthBudget =
    MonthBudget
    { id :: Maybe Integer
    , year :: Maybe Integer
    , month :: Maybe Integer
    , categoryId :: Maybe Integer
    , planned :: Maybe Integer
    , actual :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance ToTuple MonthBudget where
    toTuple :: MonthBudget -> Tuple
    toTuple (MonthBudget i y m c p a) = map toSql [i, y, m, c, p, a]

instance FromTuple MonthBudget where
    fromTuple :: Tuple -> MonthBudget
    fromTuple
        [ SqlInteger i
        , SqlInteger y
        , SqlInt32 m
        , SqlInteger c
        , SqlInteger p
        , SqlInteger a
        ] =
            MonthBudget (Just i) (Just y) (Just $ toInteger m) (Just c) (Just p) (Just a)
    fromTuple _ = empty

instance Entity MonthBudget where
    empty :: MonthBudget
    empty = MonthBudget Nothing Nothing Nothing Nothing Nothing Nothing

    pk :: MonthBudget -> Maybe Integer
    pk = DB.Entities.MonthBudget.id
    insert :: [MonthBudget] -> IO ()
    insert =
        addEnts
        (  "INSERT INTO MonthBudget (year, month, category_id, planned, actual) "
        ++ "VALUES (?, ?, ?, ?, ?);"
        )

    update :: [MonthBudget] -> IO ()
    update =
        updEnts
        ( "UPDATE Monthbudget "
        ++ "SET year = ?, month = ?, category_id = ?, planned = ?, actual = ? "
        ++ "WHERE id = ?;"
        )
        "SELECT * FROM monthbudget WHERE id = ?;"

    delete :: [MonthBudget] -> IO ()
    delete = delEnts "SELECT * FROM Monthbudget WHERE id = ?;"

    get :: Integer -> IO MonthBudget
    get = getEnt "SELECT * FROM Monthbudget WHERE id = ?;"

    getAll :: IO [MonthBudget]
    getAll = getAllEnts "SELECT * FROM Monthbudget;"


instance FromJSON MonthBudget

instance ToJSON MonthBudget where
    toEncoding = genericToEncoding defaultOptions