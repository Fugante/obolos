{-# LANGUAGE InstanceSigs #-}

module Relations.Entities
    ( Tuple
    , Category(..)
    , Transaction(..)
    , MonthBudget(..)
    , Relation(..)
    , updateTuple
    , cat2Tuple
    , tuple2Cat
    , showCat
    , showCats
    , addCat
    , getCat
    , updateCat
    , deleteCat
    , trans2Tuple
    , tuple2Trans
    , showTrans
    , addTrans
    , getTrans
    , updateTrans
    , delTrans
    , showMb
    , mb2Tuple
    , tuple2Mb
    , addMb
    , getMb
    , updateMb
    , delMb
    ) where

import Control.Monad (void)
import Data.Bool (bool)
import Data.ByteString.Char8 (unpack)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, fromJust)
import Data.Time (LocalTime)
import Database.HDBC

import qualified Relations.Queries as Q


type Tuple = [SqlValue]

rotateL :: [a] -> [a]
rotateL [] = []
rotateL as = tail as ++ [head as]

selectRight :: SqlValue -> SqlValue -> SqlValue
selectRight a b = bool a b (b /= SqlNull)

updateTuple :: Tuple -> Tuple -> Tuple
updateTuple as bs = [ f b | (f, b) <- zip (selectRight <$> as) bs ]


data Category =
    Category
    { cId :: Integer
    , cName :: String
    , cSuper :: Integer
    } deriving (Eq, Show)

showCat :: Category -> String
showCat (Category i c s) = intercalate ", " [show i, show c, show s]

showCats :: [Category] -> String
showCats cs =
       intercalate ", " ["id", "category", "supercategory"]
    ++ "\n"
    ++ intercalate "\n" (map showCat cs)

cat2Tuple :: Category -> Tuple
cat2Tuple (Category i c s) = [toSql i, toSql c, toSql s]

tuple2Cat :: Tuple -> Maybe Category
tuple2Cat [SqlInteger i, SqlString c, SqlInteger s] = Just $ Category i c s
tuple2Cat [SqlInteger i, SqlByteString c, SqlInteger s] = Just $ Category i (unpack c) s
tuple2Cat _ = Nothing

instance Semigroup Category where
    (<>) :: Category -> Category -> Category
    c <> c' = fromJust . tuple2Cat $ updateTuple (cat2Tuple c) (cat2Tuple c')

addCat :: IConnection conn => conn -> String -> Integer -> IO ()
addCat conn c s = void $ run conn Q.insertCategory [toSql c, toSql s]

getCat :: IConnection conn => conn -> Integer -> IO (Maybe Category)
getCat conn i = do
    ts <- quickQuery' conn Q.selectCategory [toSql i]
    case ts of
        [t] -> return $ tuple2Cat t
        [] -> return Nothing
        _ -> fail $ "Crital Error: more than one category with id " ++ show i

updateCat ::
    IConnection conn => conn -> Integer -> Maybe String -> Maybe Integer -> IO ()
updateCat conn i c s = do
    ts <- quickQuery' conn Q.selectCategory [toSql i]
    case ts of
        [t] ->
            let t' = updateTuple t [toSql i, toSql c, toSql s]
            in void $ run conn Q.updateCategory (rotateL t')
        _ -> putStrLn $ unwords ["Category with id", show i, "does not exist."]

deleteCat :: IConnection conn => conn -> Integer -> IO ()
deleteCat conn cid = void $ run conn Q.deleteCategory [toSql cid]


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

addTrans ::
    IConnection conn =>
    conn ->
    Integer ->
    Integer ->
    LocalTime ->
    Maybe String ->
    IO ()
addTrans conn a c d n = void $
    run conn Q.insertTransaction [toSql a, toSql c, toSql d, toSql $ fromMaybe "" n]

getTrans :: IConnection conn => conn -> Integer -> IO (Maybe Transaction)
getTrans conn i = do
    tuples <- quickQuery' conn Q.selectTransaction [toSql i]
    case tuples of
        [tuple] -> return $ tuple2Trans tuple
        [] -> return Nothing
        _ -> fail $ "Critical Error: more than one transaction with id " ++ show i

updateTrans ::
    IConnection conn =>
    conn ->
    Integer ->
    Maybe Integer ->
    Maybe Integer ->
    Maybe LocalTime ->
    Maybe String ->
    IO ()
updateTrans conn i a c d n = do
    ts <- quickQuery' conn Q.selectTransaction [toSql i]
    case ts of
        [t] ->
            let t' = updateTuple t [toSql i, toSql a, toSql c, toSql d, toSql n]
            in void $ run conn Q.updateTransaction (rotateL t')
        _ -> putStrLn $ unwords ["Transaction with id", show i, "does not exist."]

delTrans :: IConnection conn => conn -> Integer -> IO ()
delTrans conn i = void $ run conn Q.deleteTransaction [toSql i]


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
    ts <- quickQuery' conn Q.selectTransaction [toSql i]
    case ts of
        [t] ->
            let t' = updateTuple t $ map toSql [Just i, y, m, c, p, a]
            in void $ run conn Q.updateMonthBudget (rotateL t')
        _ -> putStrLn $ unwords ["Month budget with id", show i, "does not exist."]

delMb :: IConnection conn => conn -> Integer -> IO ()
delMb conn i = void $ run conn Q.deleteMonthBudget [toSql i]


data Relation =
      Cat { getCategory :: Category}
    | Trans Transaction
    | Mb MonthBudget
    deriving (Eq, Show)
