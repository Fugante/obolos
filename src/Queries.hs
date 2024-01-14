module Queries
    ( insertCategory
    , selectCategory
    , selectCategory'
    , updateCategory
    , deleteCategory
    , insertTransaction
    , selectTransaction
    , updateTransaction
    , deleteTransaction
    , insertMonthBudget
    , selectMonthBudget
    , updateMonthBudget
    , deleteMonthBudget
    ) where


-- Category CRUD queries
insertCategory :: String
insertCategory = "INSERT INTO Category (category, supercategory) VALUES (?, ?);"

selectCategory :: String
selectCategory = "SELECT * FROM Category WHERE id = ?;"

selectCategory' :: String
selectCategory' = "SELECT * FROM Category WHERE category = ? AND supercategory = ?;"

updateCategory :: String
updateCategory = "UPDATE Category SET category = ?, supercategory = ? WHERE id = ?;"

deleteCategory :: String
deleteCategory = "DELETE FROM Category WHERE id = ?"


-- Transaction CRUD queries
insertTransaction :: String
insertTransaction =
    "INSERT INTO Transaction (amount, category_id, date, notes) VALUES (?, ?, ?, ?);"

selectTransaction :: String
selectTransaction = "SELECT * FROM Transaction WHERE id = ?;"

updateTransaction :: String
updateTransaction =
       "UPDATE Transaction SET amount = ?, category_id = ?, date = ?, notes = ? "
    ++ "WHERE id = ?;"

deleteTransaction :: String
deleteTransaction = "DELETE FROM Transaction WHERE id = ?;"


-- MonthBudget CRUD queries
insertMonthBudget :: String
insertMonthBudget =
       "INSERT INTO Monthbudget (year, month, category_id, planned, actual) "
    ++ "VALUES (?, ?, ?, ?, ?);"

selectMonthBudget :: String
selectMonthBudget = "SELECT * FROM Monthbudget WHERE id = ?;"

_SELECT_MONTHBUDGET' :: String
_SELECT_MONTHBUDGET' =
    "SELECT * FROM monthbudget WHERE year = ? AND month = ? AND category_id = ?;"

updateMonthBudget :: String
updateMonthBudget =
       "UPDATE Monthbudget SET "
    ++ "year = ?, month = ?, category_id = ?, planned = ?, actual = ? "
    ++ "WHERE id = ?;"

deleteMonthBudget :: String
deleteMonthBudget = "DELETE FROM Monthbudget WHERE id = ?"