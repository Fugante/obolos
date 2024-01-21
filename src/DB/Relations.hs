module DB.Relations
    ( db
    , Tuple
    , Entity(..)
    , addEnts
    , getEnt
    , updEnt
    , updEnts
    , delEnts
    , showEnt
    )
    where

import Control.Monad ( void )
import Data.Bool ( bool )
import Data.List ( intercalate )
import Database.HDBC.PostgreSQL ( connectPostgreSQL, Connection )
import Database.HDBC

import qualified DB.Queries as Q


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

addEnts :: Entity -> [Tuple] -> IO ()
addEnts e ts = do
    conn <- db
    s <- prepare conn q
    handle conn s ts
    where
        q = case e of
            Cat -> Q.insertCategory
            Tran -> Q.insertTransaction
            Mb -> Q.insertMonthBudget

        handle :: IConnection conn => conn -> Statement -> [Tuple] -> IO ()
        handle conn s ts' =
            catchSql
            (executeMany s ts' >> commit conn)
            $ \(SqlError _ _ m) -> print m >> rollback conn

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

updEnts :: Entity -> [Tuple] -> IO ()
updEnts e ts = do
    conn <- db
    mapM_ (handle conn) ts
    where
        (sq, uq) = case e of
            Cat -> (Q.selectCategory, Q.updateCategory)
            Tran -> (Q.selectTransaction, Q.updateTransaction)
            Mb -> (Q.selectMonthBudget, Q.updateMonthBudget)

        upd :: IConnection conn => conn -> Tuple -> IO ()
        upd conn t = do
            ts' <- quickQuery' conn sq [head t]
            case ts' of
                [t'] -> void $ run conn uq (rotateL $ setNewAttrs t' t)
                _ -> return ()

        handle :: IConnection conn => conn -> Tuple -> IO ()
        handle conn t =
            catchSql
            (upd conn t >> commit conn)
            $ \(SqlError _ _ m) -> print m >> rollback conn

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
        [t'] -> do
            _ <- run conn q' (rotateL $ setNewAttrs t' t)
            commit conn
        _ -> putStrLn $ unwords ["Category with id", show i, "does not exist."]

delEnts :: Entity -> [Tuple] -> IO ()
delEnts e ts = do
    conn <- db
    q <-
        case e of
            Cat -> return Q.deleteCategory
            Tran -> return Q.deleteTransaction
            Mb -> return Q.deleteMonthBudget
    s <- prepare conn q
    executeMany s ts
    commit conn

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