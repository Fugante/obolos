module Models
    ( Category(..)
    , showCategory
    , cat2Tuple
    , tuple2Cat
    , updateTuple
    , addCategory
    , getCategory
    , updateCategory
    , deleteCategory
    -- , addTransaction
    -- , getTransaction
    -- , updateTransaction
    -- , deleteTransaction
    -- , addMonthBudget
    ) where

import Control.Monad (void)
import Data.Bool (bool)
import Data.ByteString.Char8 (unpack)
import Data.Maybe (fromJust)
import Database.HDBC


type Tuple = [SqlValue]

data Category =
    Category
    { cId :: Integer
    , cName :: String
    , cSuper :: Integer
    } deriving (Eq, Show)

showCategory :: Category -> String
showCategory (Category cid cn cs) = unwords [show cid, show cn, show cs]

cat2Tuple :: Category -> Tuple
cat2Tuple (Category cid cn cs) = [toSql cid, toSql cn, toSql cs]

tuple2Cat :: Tuple -> Maybe Category
tuple2Cat [SqlInteger cid, SqlByteString c, SqlInteger sc] =
    Just $ Category cid (unpack c) sc
tuple2Cat _ = Nothing

selectRight :: SqlValue -> SqlValue -> SqlValue
selectRight a b = bool a b (b /= SqlNull)

updateTuple :: [SqlValue] -> [SqlValue] -> [SqlValue]
updateTuple as bs = [ f b | (f, b) <- zip (selectRight <$> as) bs ]


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


addCategory :: IConnection conn => conn -> String -> Integer -> IO ()
addCategory conn c sc = void $ run conn _INSERT_CATEGORY [toSql c, toSql sc]

getCategory :: IConnection conn => conn -> Integer -> IO (Maybe Category)
getCategory conn cid = do
    tuples <- quickQuery' conn _SELECT_CATEGORY [toSql cid]
    case tuples of
        [tuple] -> return $ tuple2Cat tuple
        [] -> return Nothing
        _ -> fail $ "Crital Error: more than one category with id " ++ show cid

updateCategory ::
    IConnection conn => conn -> Integer -> Maybe String -> Maybe Integer -> IO ()
updateCategory conn cid c sc = do
    mCat <- getCategory conn cid
    case mCat of
        Nothing -> putStrLn $ unwords ["Category with id", show cid, "does not exist."]
        Just cat -> void $ run conn _UPDATE_CATEGORY [c', sc', cid']
            where [cid', c', sc'] =
                    updateTuple (cat2Tuple cat) [toSql cid, toSql c, toSql sc]

deleteCategory :: IConnection conn => conn -> Integer -> IO ()
deleteCategory conn cid = void $ run conn _DELETE_CATEGORY [toSql cid]

-- addTransaction :: IConnection conn => conn -> Transaction -> IO ()
-- addTransaction conn (Transaction _ a cid d n) =
--     void $ run conn _INSERT_TRANSACTION [toSql a, toSql cid, toSql d, toSql n]

-- getTransaction :: IConnection conn => conn -> Integer -> IO (Maybe Transaction)
-- getTransaction conn tid = do
--     tuples <- quickQuery' conn _SELECT_TRANSACTION [toSql tid]
--     case tuples of
--         [[_, a, cid, d, n]] -> return $ Just $
--             Transaction tid (fromSql a) (fromSql cid) (fromSql d) (fromSql n)
--         [] -> return Nothing
--         _ -> fail $ "Critical Error: more than one transaction with id " ++ show tid

-- updateTransaction :: IConnection conn => conn -> Transaction -> IO (Maybe Transaction)
-- updateTransaction conn (Transaction tid a cid d n) =
--     run conn _UPDATE_CATEGORY [toSql a, toSql cid, toSql d, toSql n, toSql tid]
--     >> getTransaction conn tid

-- deleteTransaction :: IConnection conn => conn -> Integer -> IO Integer
-- deleteTransaction conn tid = run conn _DELETE_CATEGORY [toSql tid]

-- addMonthBudget :: IConnection conn => conn -> MonthBudget -> IO MonthBudget
-- addMonthBudget conn (MonthBudget _ y m cid p a) = do
--     run conn _INSERT_MONTHBUDGET [toSql y, toSql m, toSql cid, toSql p, toSql a]
--     tuples <- quickQuery' conn _SELECT_MONTHBUDGET' [toSql y, toSql m, toSql cid]
--     case tuples of
--         [mid:_] -> return $ MonthBudget (fromSql mid) y m cid p a
--         [] -> fail "Error: could not insert monthbudget"
--         _ -> fail "Critical error"