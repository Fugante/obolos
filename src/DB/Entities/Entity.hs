module DB.Entities.Entity
    ( Tuple
    , FromTuple(..)
    , ToTuple(..)
    , Entity(..)
    , db
    , addEnts
    , updEnts
    , delEnts
    , getEnt
    , getAllEnts
    ) where

-- Base libraries
import Control.Monad (void)
import Data.Bool (bool)

-- External dependencies
import Database.HDBC
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)


type Tuple = [SqlValue]

type SqlStr = String

class ToTuple a where
    toTuple :: a -> Tuple

class FromTuple a where
    fromTuple :: Tuple -> a

class (ToTuple a, FromTuple a) => Entity a where
    empty :: a
    pk :: a -> Maybe Integer
    insert :: [a] -> IO ()
    update :: [a] -> IO ()
    delete :: [a] -> IO ()
    get :: Integer -> IO a
    getAll :: IO [a]


addEnts :: Entity e => SqlStr -> [e] -> IO ()
addEnts q es = do
    conn <- db
    s <- prepare conn q
    catchSql
        (executeMany s (map toTuple es) >> commit conn)
        (\(SqlError _ _ m) -> print m >> rollback conn)

updEnts :: Entity e => SqlStr -> SqlStr -> [e] -> IO ()
updEnts sq uq es = do
    conn <- db
    let
        handle :: Entity e => e -> IO ()
        handle e =
            catchSql
            (upd e >> commit conn)
            (\(SqlError _ _ m) -> print m >> rollback conn)

        upd :: Entity e => e -> IO ()
        upd e = do
            ts <- quickQuery conn sq [toSql (pk e)]
            case ts of
                [t] -> void $ run conn uq (rotateL (setNewAttrs t (toTuple e)))
                _ -> return ()

        rotateL :: [a] -> [a]
        rotateL [] = []
        rotateL as = tail as ++ [head as]

        setNewAttrs :: Tuple -> Tuple -> Tuple
        setNewAttrs as bs = [ f b | (f, b) <- zip (selectRight <$> as) bs ]

        selectRight :: SqlValue -> SqlValue -> SqlValue
        selectRight a b = bool a b (b /= SqlNull)

    mapM_ handle es

delEnts :: Entity e => SqlStr -> [e] -> IO ()
delEnts q es = do
    conn <- db
    let ts = map ((:[]) . toSql . pk) es
    s <- prepare conn q
    executeMany s ts
    commit conn

getEnt :: Entity e => SqlStr -> Integer -> IO e
getEnt q i = do
    conn <- db
    ts <- quickQuery' conn q [toSql i]
    case ts of
        [t] -> return $ fromTuple t
        _ -> return empty

getAllEnts :: Entity e => SqlStr -> IO [e]
getAllEnts q = do
    conn <- db
    ts <- quickQuery' conn q []
    return $ map fromTuple ts


db :: IO Connection
db = connectPostgreSQL "user=obolos password=hidragerum dbname=Obolos host=localhost port=5423"