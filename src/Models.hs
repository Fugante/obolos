{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Models
    ( Category(..)
    , Transaction(..)
    , MonthBudget(..)
    , addCategory
    , getCategory
    , updateCategory
    , deleteCategory
    , addTransaction
    , getTransaction
    , updateTransaction
    , deleteTransaction
    , addMonthBudget
    ) where

import Control.Monad (void)
import Data.Time
import Database.HDBC
    ( fromSql, toSql, quickQuery', IConnection(run) )


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
_INSERT_CATEGORY = "INSERT INTO Category (category, supercategory) VALUES (?, ?);"

_SELECT_CATEGORY :: String
_SELECT_CATEGORY = "SELECT * FROM Category WHERE id = ?;"

_SELECT_CATEGORY' :: String
_SELECT_CATEGORY' = "SELECT * FROM Category WHERE category = ? AND supercategory = ?;"

_UPDATE_CATEGORY :: String
_UPDATE_CATEGORY = "UPDATE Category SET category = ?, supercategory = ? WHERE id = ?;"

_DELETE_CATEGORY :: String
_DELETE_CATEGORY = "DELETE FROM Category WHERE id = ?"

_INSERT_TRANSACTION :: String
_INSERT_TRANSACTION =
    "INSERT INTO transaction (amount, category_id, date, notes) VALUES (?, ?, ?, ?);"

_SELECT_TRANSACTION :: String
_SELECT_TRANSACTION = "SELECT * FROM transaction WHERE id = ?;"

_UPDATE_TRANSACTION :: String
_UPDATE_TRANSACTION =
       "UPDATE transaction SET amount = ?, category = ?, date = ?, notes = ? "
    ++ "WHERE id = ?;"

_DELETE_TRANSACTION :: String
_DELETE_TRANSACTION = "DELETE FROM transaction WHERE id = ?"

_INSERT_MONTHBUDGET :: String
_INSERT_MONTHBUDGET =
       "INSERT INTO monthbudget (year, month, category_id, planned, actual) "
    ++ "VALUES (?, ?, ?, ?, ?);"

_SELECT_MONTHBUDGET :: String
_SELECT_MONTHBUDGET = "SELECT * FROM monthbudget WHERE id = ?;"

_SELECT_MONTHBUDGET' :: String
_SELECT_MONTHBUDGET' =
    "SELECT * FROM monthbudget WHERE year = ? AND month = ? AND category_id = ?;"

_UPDATE_MONTHBUDGET :: String
_UPDATE_MONTHBUDGET =
       "UPDATE monthbudget SET "
    ++ "year = ?, month = ?, category_id = ?, planned = ? actual = ? "
    ++ "WHERE id = ?;"

_DELETE_MONTHBUDGET :: String
_DELETE_MONTHBUDGET = "DELETE FROM monthbudget WHERE id = ?"

addCategory :: IConnection conn => conn -> Category -> IO ()
addCategory conn (Category _ c sc) = void $ run conn _INSERT_CATEGORY [toSql c, toSql sc]

getCategory :: IConnection conn => conn -> Integer -> IO (Maybe Category)
getCategory conn cid = do
    tuples <- quickQuery' conn _SELECT_CATEGORY [toSql cid]
    case tuples of
        [[_, cat, sup]] -> return $ Just $ Category cid (fromSql cat) (fromSql sup)
        [] -> return Nothing
        _ -> fail $ "Crital Error: more than one category with id " ++ show cid

updateCategory :: IConnection conn => conn -> Category -> IO ()
updateCategory conn (Category cid c sc) =
    void $ run conn _UPDATE_CATEGORY [toSql c, toSql sc, toSql cid]

deleteCategory :: IConnection conn => conn -> Integer -> IO ()
deleteCategory conn cid = void $ run conn _DELETE_CATEGORY [toSql cid]

addTransaction :: IConnection conn => conn -> Transaction -> IO Transaction
addTransaction conn (Transaction _ a cid d n) = do
    run conn _INSERT_TRANSACTION attrs
    tuples <- quickQuery' conn _SELECT_CATEGORY' attrs
    case tuples of
        [tid:_] -> return $ Transaction (fromSql tid) a cid d n
        [] -> fail "Error: could not insert transaction"
        _ -> fail "Critical error: more than one category with same attributes"
        where attrs = [toSql a, toSql cid, toSql d, toSql n]

getTransaction :: IConnection conn => conn -> Integer -> IO (Maybe Transaction)
getTransaction conn tid = do
    tuples <- quickQuery' conn _SELECT_TRANSACTION [toSql tid]
    case tuples of
        [[_, a, cid, d, n]] -> return $ Just $
            Transaction tid (fromSql a) (fromSql cid) (fromSql d) (fromSql n)
        [] -> return Nothing
        _ -> fail $ "Critical Error: more than one transaction with id " ++ show tid

updateTransaction :: IConnection conn => conn -> Transaction -> IO (Maybe Transaction)
updateTransaction conn (Transaction tid a cid d n) =
    run conn _UPDATE_CATEGORY [toSql a, toSql cid, toSql d, toSql n, toSql tid]
    >> getTransaction conn tid

deleteTransaction :: IConnection conn => conn -> Integer -> IO Integer
deleteTransaction conn tid = run conn _DELETE_CATEGORY [toSql tid]

addMonthBudget :: IConnection conn => conn -> MonthBudget -> IO MonthBudget
addMonthBudget conn (MonthBudget _ y m cid p a) = do
    run conn _INSERT_MONTHBUDGET [toSql y, toSql m, toSql cid, toSql p, toSql a]
    tuples <- quickQuery' conn _SELECT_MONTHBUDGET' [toSql y, toSql m, toSql cid]
    case tuples of
        [mid:_] -> return $ MonthBudget (fromSql mid) y m cid p a
        [] -> fail "Error: could not insert monthbudget"
        _ -> fail "Critical error"