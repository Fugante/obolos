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
    "INSERT INTO transaction (amount, category_id, date, notes) VALUES (?, ?, ?, ?);"

selectTransaction :: String
selectTransaction = "SELECT * FROM transaction WHERE id = ?;"

updateTransaction :: String
updateTransaction =
       "UPDATE transaction SET amount = ?, category_id = ?, date = ?, notes = ? "
    ++ "WHERE id = ?;"

deleteTransaction :: String
deleteTransaction = "DELETE FROM transaction WHERE id = ?"


-- MonthBudget CRUD queries
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