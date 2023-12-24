{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Models where

import Data.Time
import Database.HDBC


data Category =
    Category
    { catId :: Integer
    , category :: String
    , supercategory :: Integer
    }
    deriving (Eq, Show, Read)

data Transaction =
    Transaction
    { transId :: Integer
    , amount :: Integer
    , transCat :: Integer
    , date :: UTCTime
    , notes :: String
    }
    deriving (Eq, Show, Read)

data MonthBudget =
    MonthBudget
    { mbId :: Integer
    , year :: Integer
    , month :: Integer
    , mbCat :: Integer
    , planned :: Integer
    , actual :: Integer
    }
    deriving (Eq, Show, Read)

_INSERT_CATEGORY :: String
_INSERT_CATEGORY = "INSERT INTO category (category, supercategory) VALUES (?, ?);"

_SELECT_CATEGORY :: String
_SELECT_CATEGORY = "SELECT * FROM category WHERE id = ?;"

_SELECT_CATEGORY' :: String
_SELECT_CATEGORY' = "SELECT * FROM category WHERE category = ? AND supercategory = ?;"

_DELETE_CATEGORY :: String
_DELETE_CATEGORY = "DELETE FROM category WHERE id = ?"

_UPDATE_CATEGORY :: String
_UPDATE_CATEGORY = "UPDATE category SET category = ?, supercategory = ? WHERE id = ?;"

_INSERT_TRANSACTION :: String
_INSERT_TRANSACTION =
    "INSERT INTO transaction (amount, transCat, date, notes) VALUES (?, ?, ?, ?);"

_SELECT_TRANSACTION :: String
_SELECT_TRANSACTION = "SELECT * FROM transaction WHERE id = ?;"

_UPDATE_TRANSACTION :: String
_UPDATE_TRANSACTION =
    "UPDATE transaction SET amount = ?, category = ?, date = ?, notes = ? WHERE id = ?;"

_DELETE_TRANSACTION :: String
_DELETE_TRANSACTION = "DELETE FROM transaction WHERE id = ?"

addCategory :: IConnection conn => conn -> Category -> IO Category
addCategory conn (Category _ c sc) = do
    run conn _INSERT_CATEGORY attrs
    tuples <- quickQuery' conn _SELECT_CATEGORY' attrs
    case tuples of
        [cid:_] -> return $ Category (fromSql cid) c sc
        [] -> fail "Error: could not add new category"
        _ -> fail "Critical error: more than one category with same name and supercategory"
        where attrs = [toSql c, toSql sc]

getCategory :: IConnection conn => conn -> Integer -> IO (Maybe Category)
getCategory conn cid = do
    tuples <- quickQuery' conn _SELECT_CATEGORY [toSql cid]
    case tuples of
        [[_, cat, sup]] -> return $ Just $ Category cid (fromSql cat) (fromSql sup)
        [] -> return Nothing
        _ -> fail $ "Crital Error: more than one category with id " ++ show cid

updateCategory :: IConnection conn => conn -> Category -> IO (Maybe Category)
updateCategory conn (Category cid c sc) = do
    run conn _UPDATE_CATEGORY [toSql c, toSql sc, toSql cid]
    getCategory conn cid

deleteCategory :: IConnection conn => conn -> Integer -> IO Integer
deleteCategory conn cid = run conn _DELETE_CATEGORY [toSql cid]

addTransaction :: IConnection conn => conn -> Transaction -> IO Transaction
addTransaction conn (Transaction _ a tr d n) = do
    run conn _INSERT_TRANSACTION attrs
    tuples <- quickQuery' conn _SELECT_CATEGORY' attrs
    case tuples of
        [tid:_] -> return $ Transaction (fromSql tid) a tr d n -- THIS MAY GO WRONG
        [] -> fail "Error: could not insert transaction"
        _ -> fail "Critical error: more than one category with same attributes"
        where attrs = [toSql a, toSql tr, toSql d, toSql n]

getTransaction :: IConnection conn => conn -> Integer -> IO (Maybe Transaction)
getTransaction conn tid = do
    tuples <- quickQuery' conn _SELECT_TRANSACTION [toSql tid]
    case tuples of
        [[_, a, tr, d, n]] -> return $ Just $
            Transaction tid (fromSql a) (fromSql tr) (fromSql d) (fromSql n)
        [] -> return Nothing
        _ -> fail $ "Critical Error: more than one transaction with id " ++ show tid

updateTransaction :: IConnection conn => conn -> Transaction -> IO (Maybe Transaction)
updateTransaction conn (Transaction tid a tr d n) =
    run conn _UPDATE_CATEGORY [toSql a, toSql tr, toSql d, toSql n, toSql tid] >>
    getTransaction conn tid

deleteTransaction :: IConnection conn => conn -> Integer -> IO Integer
deleteTransaction conn tid = run conn _DELETE_CATEGORY [toSql tid]