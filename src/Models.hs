module Models
    ( Category(..)
    , Transaction(..)
    , updateTuple
    , cat2Tuple
    , tuple2Cat
    , showCategory
    , addCategory
    , getCategory
    , updateCategory
    , deleteCategory
    , trans2Tuple
    , tuple2Trans
    , showTrans
    , addTransaction
    , getTransaction
    , updateTransaction
    , deleteTransaction
    , showMb
    , mb2Tuple
    , tuple2Mb
    , addMb
    , getMb
    , updateMb
    , deleteMb
    ) where

import Control.Monad (void)
import Data.Bool (bool)
import Data.ByteString.Char8 (unpack)
import Data.Maybe (fromMaybe)
import Data.Time (LocalTime)
import Database.HDBC

import qualified Queries as Q


type Tuple = [SqlValue]

selectRight :: SqlValue -> SqlValue -> SqlValue
selectRight a b = bool a b (b /= SqlNull)

updateTuple :: [SqlValue] -> [SqlValue] -> [SqlValue]
updateTuple as bs = [ f b | (f, b) <- zip (selectRight <$> as) bs ]


data Category =
    Category
    { cId :: Integer
    , cName :: String
    , cSuper :: Integer
    } deriving (Eq, Show)

showCategory :: Category -> String
showCategory (Category i c s) = unwords [show i, show c, show s]

cat2Tuple :: Category -> Tuple
cat2Tuple (Category i c s) = [toSql i, toSql c, toSql s]

tuple2Cat :: Tuple -> Maybe Category
tuple2Cat [SqlInteger i, SqlByteString c, SqlInteger s] =
    Just $ Category i (unpack c) s
tuple2Cat _ = Nothing

addCategory :: IConnection conn => conn -> String -> Integer -> IO ()
addCategory conn c s = void $ run conn Q.insertCategory [toSql c, toSql s]

getCategory :: IConnection conn => conn -> Integer -> IO (Maybe Category)
getCategory conn i = do
    tuples <- quickQuery' conn Q.selectCategory [toSql i]
    case tuples of
        [tuple] -> return $ tuple2Cat tuple
        [] -> return Nothing
        _ -> fail $ "Crital Error: more than one category with id " ++ show i

updateCategory ::
    IConnection conn => conn -> Integer -> Maybe String -> Maybe Integer -> IO ()
updateCategory conn i c s = do
    mCat <- getCategory conn i
    case mCat of
        Nothing -> putStrLn $ unwords ["Category with id", show i, "does not exist."]
        Just cat ->
            let [i', c', s'] = updateTuple (cat2Tuple cat) [toSql i, toSql c, toSql s]
            in void $ run conn Q.updateCategory [c', s', i']

deleteCategory :: IConnection conn => conn -> Integer -> IO ()
deleteCategory conn cid = void $ run conn Q.deleteCategory [toSql cid]


data Transaction =
    Transaction
    { tId :: Integer
    , tAmount :: Integer
    , tCat :: Integer
    , tDate :: LocalTime
    , tNotes :: String
    } deriving (Eq, Show)

showTrans :: Transaction -> String
showTrans (Transaction i a c d n) = unwords [show i, show a, show c, show d, n]

trans2Tuple :: Transaction -> Tuple
trans2Tuple (Transaction i a c d n) = [toSql i, toSql a, toSql c, toSql d, toSql n]

tuple2Trans :: Tuple -> Maybe Transaction
tuple2Trans [SqlInteger i, SqlInteger a, SqlInteger c, SqlLocalTime d, SqlByteString n] =
    Just $ Transaction i a c d (unpack n)
tuple2Trans _ = Nothing

addTransaction ::
    IConnection conn =>
    conn ->
    Integer ->
    Integer ->
    LocalTime ->
    Maybe String ->
    IO ()
addTransaction conn a c d n = void $
    run conn Q.insertTransaction [toSql a, toSql c, toSql d, toSql $ fromMaybe "" n]

getTransaction :: IConnection conn => conn -> Integer -> IO (Maybe Transaction)
getTransaction conn i = do
    tuples <- quickQuery' conn Q.selectTransaction [toSql i]
    case tuples of
        [tuple] -> return $ tuple2Trans tuple
        [] -> return Nothing
        _ -> fail $ "Critical Error: more than one transaction with id " ++ show i

updateTransaction ::
    IConnection conn =>
    conn ->
    Integer ->
    Maybe Integer ->
    Maybe Integer ->
    Maybe LocalTime ->
    Maybe String ->
    IO ()
updateTransaction conn i a c d n = do
    mTrans <- getTransaction conn i
    case mTrans of
        Nothing -> putStrLn $ unwords ["Transaction with id", show i, "does not exist."]
        Just trans ->
            let [i', a', c', d', n'] =
                    updateTuple
                    (trans2Tuple trans)
                    [toSql i, toSql a, toSql c, toSql d, toSql n]
            in void $ run conn Q.updateTransaction [a', c', d', n', i']

deleteTransaction :: IConnection conn => conn -> Integer -> IO ()
deleteTransaction conn i = void $ run conn Q.deleteTransaction [toSql i]


data MonthBudget =
    MonthBudget
    { mId :: Integer
    , mYear :: Integer
    , mMonth :: Integer
    , mCat :: Integer
    , mPlanned :: Integer
    , mActual :: Integer
    } deriving (Eq, Show)

showMb :: MonthBudget -> String
showMb (MonthBudget i y m c p a) = unwords $ map show [i, y, m, c, p, a]

mb2Tuple :: MonthBudget -> Tuple
mb2Tuple (MonthBudget i y m c p a) = map toSql [i, y, m, c, p, a]

tuple2Mb :: Tuple -> Maybe MonthBudget
tuple2Mb
    [ SqlInteger i
    , SqlInteger y
    , SqlInt32 m
    , SqlInteger c
    , SqlInteger p
    , SqlInteger a
    ] =
    Just $ MonthBudget i y (toInteger m) c p a
tuple2Mb _ = Nothing

addMb ::
    IConnection conn =>
    conn ->
    Integer ->
    Integer ->
    Integer ->
    Integer ->
    Integer ->
    IO ()
addMb conn y m c p a = void $ run conn Q.insertMonthBudget $ map toSql [y, m, c, p, a]

getMb :: IConnection conn => conn -> Integer -> IO (Maybe MonthBudget)
getMb conn i = do
    tuples <- quickQuery' conn Q.selectMonthBudget [toSql i]
    case tuples of
        [tuple] -> return $ tuple2Mb tuple
        [] -> return Nothing
        _ -> fail $ "Critical Error: more than one monthbudget with id " ++ show i

updateMb ::
    IConnection conn =>
    conn ->
    Integer ->
    Maybe Integer ->
    Maybe Integer ->
    Maybe Integer ->
    Maybe Integer ->
    Maybe Integer ->
    IO ()
updateMb conn i y m c p a = do
    mMb <- getMb conn i
    case mMb of
        Nothing -> putStrLn $ unwords ["Month budget with id", show i, "does not exist."]
        Just mb ->
            let [i', y', m', c', p', a'] =
                    updateTuple (mb2Tuple mb) (map toSql [Just i, y, m, c, p, a])
            in void $ run conn Q.updateMonthBudget [y', m', c', p', a', i']

deleteMb :: IConnection conn => conn -> Integer -> IO ()
deleteMb conn i = void $ run conn Q.deleteMonthBudget [toSql i]