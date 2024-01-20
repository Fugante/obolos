module DB.Views
    ( getAll
    ) where

import Database.HDBC ( quickQuery' )

import DB.Relations ( Entity(..), Tuple, db )
import qualified DB.Queries as Q


getAll :: Entity -> IO [Tuple]
getAll e = do
    conn <- db
    q <-
        case e of
            Cat -> return $ Q.selectAll ++ "Category;"
            Tran -> return $ Q.selectAll ++ "Transaction;"
            Mb -> return $ Q.selectAll ++ "Monthbudget;"
    quickQuery' conn q []