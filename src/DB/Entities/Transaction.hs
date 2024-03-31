{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module DB.Entities.Transaction
    ( Transaction(..)
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
import Data.ByteString.Char8 (unpack)
import Data.Time (LocalTime)


data Transaction =
    Transaction
    { id :: Maybe Integer
    , amount :: Maybe Integer
    , category_id :: Maybe Integer
    , date :: Maybe LocalTime
    , notes :: Maybe String
    } deriving (Eq, Show, Generic)

instance ToTuple Transaction where
    toTuple :: Transaction -> Tuple
    toTuple (Transaction i a c d n) = [toSql i, toSql a, toSql c, toSql d, toSql n]

instance FromTuple Transaction where
    fromTuple :: Tuple -> Transaction
    fromTuple
        [ SqlInteger i
        , SqlInteger a
        , SqlInteger c
        , SqlLocalTime d
        , SqlByteString n
        ] =
            Transaction (Just i) (Just a) (Just c) (Just d) (Just $ unpack n)
    fromTuple _ = empty

instance Entity Transaction where
    empty :: Transaction
    empty = Transaction Nothing Nothing Nothing Nothing Nothing

    pk :: Transaction -> Maybe Integer
    pk = DB.Entities.Transaction.id

    insert :: [Transaction] -> IO ()
    insert = addEnts
        "INSERT INTO Transaction (amount, category_id, date, notes) VALUES (?, ?, ?, ?);"

    update :: [Transaction] -> IO ()
    update =
        updEnts
        (   "UPDATE Transaction "
        ++ "SET amount = ?, category_id = ?, date = ?, notes = ? "
        ++ "WHERE id = ?;"
        )
        "SELECT * FROM Transaction WHERE id = ?"

    delete :: [Transaction] -> IO ()
    delete = delEnts "DELETE FROM Transaction WHERE id = ?;"

    get :: Integer -> IO Transaction
    get = getEnt "SELECT * FROM Transaction WHERE id = ?;"

    getAll :: IO [Transaction]
    getAll = getAllEnts "SELECT * FROM Transaction;"


instance FromJSON Transaction

instance ToJSON Transaction where
    toEncoding = genericToEncoding defaultOptions