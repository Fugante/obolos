{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module DB.Entities.Category
    ( Category(..)
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


data Category =
    Category
    { id :: Maybe Integer
    , category :: Maybe String
    , supercategory :: Maybe Integer
    } deriving (Eq, Show, Generic)

instance ToTuple Category where
    toTuple :: Category -> Tuple
    toTuple (Category i c s) = [toSql i, toSql c, toSql s]

instance FromTuple Category where
    fromTuple :: Tuple -> Category
    fromTuple [SqlInteger i, SqlByteString c, SqlInteger s] =
        Category (Just i) (Just $ unpack c) (Just s)
    fromTuple _ = empty

instance Entity Category where
    empty :: Category
    empty = Category Nothing Nothing Nothing

    pk :: Category -> Maybe Integer
    pk = DB.Entities.Category.id

    insert :: [Category] -> IO ()
    insert = addEnts "INSERT INTO Category (category, supercategory) VALUES (?, ?);"

    update:: [Category] -> IO ()
    update =
        updEnts
        (  "UPDATE Category "
        ++ "SET category = ?, supercategory = ?"
        ++ "WHERE id = ?;"
        )
        "SELECT * FROM Category WHERE id = ?;"

    delete :: [Category] -> IO ()
    delete = delEnts "DELETE FROM Category WHERE id = ?;"

    get :: Integer -> IO Category
    get = getEnt "SELECT * FROM Category WHERE id = ?;"

    getAll :: IO [Category]
    getAll = getAllEnts "SELECT * FROM Category;"


instance FromJSON Category

instance ToJSON Category where
    toEncoding = genericToEncoding defaultOptions