module Relations.Views
    ( getAll
    ) where


import Relations.Entities
import Database.HDBC
import Data.Maybe (fromJust)
import qualified Relations.Queries as Q


getAll :: IConnection conn => conn -> Relation -> IO [Relation]
getAll conn (Cat _) = do
    tuples <- quickQuery' conn (Q.selectAll ++ "Category;") []
    return $ map (Cat . fromJust . tuple2Cat) tuples
getAll conn (Trans _) = do
    tuples <- quickQuery' conn (Q.selectAll ++ "Transaction;") []
    return $ map (Trans . fromJust . tuple2Trans) tuples
getAll conn (Mb _) = do
    tuples <- quickQuery' conn (Q.selectAll ++ "Monthbudget;") []
    return $ map (Mb . fromJust . tuple2Mb) tuples