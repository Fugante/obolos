module Relations.Entities
    ( db
    , Tuple
    , Entity(..)
    , addEnt
    , getEnt
    , updEnt
    , delEnt
    , showEnt
    ) where

import Control.Monad ( void )
import Data.Bool ( bool )
import Data.List ( intercalate )
import Database.HDBC.PostgreSQL ( connectPostgreSQL, Connection )
import Database.HDBC
    ( toSql
    , quickQuery'
    , SqlValue(SqlNull, SqlInteger, SqlByteString, SqlInt32, SqlLocalTime)
    , IConnection(run)
    )

import qualified Relations.Queries as Q


db :: IO Connection
db = connectPostgreSQL "user=obolos password=hidragerum dbname=Obolos host=localhost port=5423"

type Tuple = [SqlValue]

rotateL :: [a] -> [a]
rotateL [] = []
rotateL as = tail as ++ [head as]

selectRight :: SqlValue -> SqlValue -> SqlValue
selectRight a b = bool a b (b /= SqlNull)

setNewAttrs :: Tuple -> Tuple -> Tuple
setNewAttrs as bs = [ f b | (f, b) <- zip (selectRight <$> as) bs ]


data Entity = Cat | Tran | Mb

addEnt :: Entity -> Tuple -> IO ()
addEnt e t = do
    conn <- db
    q <-
        case e of
            Cat -> return Q.insertCategory
            Tran -> return Q.insertTransaction
            Mb -> return Q.insertMonthBudget
    void $ run conn q t

getEnt :: Entity -> Integer -> IO Tuple
getEnt e i = do
    let i' = [toSql i]
    conn <- db
    q <-
        case e of
            Cat -> return Q.selectCategory
            Tran -> return Q.selectTransaction
            Mb -> return Q.selectMonthBudget
    ts <- quickQuery' conn q i'
    case ts of
        [t] -> return t
        [] -> return []
        _ -> fail $ "Crital Error: more than one category with id " ++ show i

updEnt :: Entity -> Integer -> Tuple -> IO ()
updEnt e i t = do
    let i' = [toSql i]
    conn <- db
    (q, q') <-
        case e of
            Cat -> return (Q.selectCategory, Q.updateCategory)
            Tran -> return (Q.selectTransaction, Q.updateTransaction)
            Mb -> return (Q.selectMonthBudget, Q.updateMonthBudget)
    ts <- quickQuery' conn q i'
    case ts of
        [t'] -> void $ run conn q' (rotateL $ setNewAttrs t' t)
        _ -> putStrLn $ unwords ["Category with id", show i, "does not exist."]

delEnt :: Entity -> Integer -> IO ()
delEnt e i = do
    conn <- db
    q <-
        case e of
            Cat -> return Q.deleteCategory
            Tran -> return Q.deleteTransaction
            Mb -> return Q.deleteMonthBudget
    void $ run conn q [toSql i]


showEnt :: Entity -> Tuple -> String
showEnt
    Cat
    [ SqlInteger i
    , SqlByteString c
    , SqlInteger s
    ] =
        intercalate ", " [show i, show c, show s]
showEnt
    Tran
    [ SqlInteger i
    , SqlInteger a
    , SqlInteger c
    , SqlLocalTime d
    , SqlByteString n
    ] =
        intercalate ", " [show i, show a, show c, show d, show n]
showEnt
    Mb
    [ SqlInteger i
    , SqlInteger y
    , SqlInt32 m
    , SqlInteger c
    , SqlInteger p
    , SqlInteger a
    ] =
        intercalate ", " [show i, show y, show m, show c, show p, show a]
showEnt _ _ = ""