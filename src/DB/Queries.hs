module DB.Queries
    ( insertCategory
    , selectCategory
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
    , selectAll
    , monthTotals
    ) where


-- Category CRUD queries

insertCategory :: String
insertCategory =
    "INSERT INTO Category (category, supercategory) VALUES (?, ?) ON CONFLICT DO NOTHING;"

selectCategory :: String
selectCategory = "SELECT * FROM Category WHERE id = ?;"

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
deleteMonthBudget = "DELETE FROM Monthbudget WHERE id = ?;"



selectAll :: String
selectAll = "SELECT * FROM "



monthTotals :: String
monthTotals =
       "SELECT c.category, m.planned, m.actual, m.planned - m.actual AS total "
    ++ "FROM monthbudget AS m "
    ++ "JOIN category AS c ON m.category_id = c.id "
    ++ "WHERE m.year = ? AND m.month = ?;"