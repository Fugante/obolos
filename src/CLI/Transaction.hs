module CLI.Transaction
    ( TransactionOptions(..)
    , transactionOptions
    , handleTransaction
    ) where

import Data.Time
import Database.HDBC (IConnection)
import Options.Applicative

import Models


data AddOptions =
    AddOptions
    { addOptAmount :: Integer
    , addOptCat :: Integer
    , addOptDate :: LocalTime
    , addOptNotes :: Maybe String
    }

data GetOptions = GetOptions { getOptId :: Integer }

data UpdateOptions =
    UpdateOptions
    { upOptId :: Integer
    , upOptAmount :: Maybe Integer
    , upOptCat :: Maybe Integer
    , upOptDate :: Maybe LocalTime
    , upOptNotes :: Maybe String
    }

data DeleteOptions = DeleteOptions { delOptId :: Integer}

data TransactionOptions =
      Add AddOptions
    | Get GetOptions
    | Update UpdateOptions
    | Delete DeleteOptions


-- Arguments and options

idArg :: Parser Integer
idArg = argument auto (metavar "ID")

amountArg :: Parser Integer
amountArg = argument auto (metavar "AMOUNT" <> help "transaction amount (in cents)")

categoryArg :: Parser Integer
categoryArg =
    argument auto (metavar "CATEGORY-ID" <> help "id of the transaction's category")

dateArg :: Parser LocalTime
dateArg =
    argument
    auto
    (  metavar "DATETIME"
    <> help "date and time of the transaction. Format: yyyy-mm-dd HH:MM:SS[.ffffff]"
    )

amountOpt :: Parser (Maybe Integer)
amountOpt = optional $
    option
    auto
    (  short 'a'
    <> long "amount"
    <> help "amount (in cents) of the transaction"
    <> metavar "AMOUNT"
    )

categoryOpt :: Parser (Maybe Integer)
categoryOpt = optional $
    option
    auto
    (  short 'c'
    <> long "category-id"
    <> help "id of the transaction's category"
    <> metavar "CATEGORY-ID"
    )

dateOpt :: Parser (Maybe LocalTime)
dateOpt = optional $
    option
    auto
    (  short 'd'
    <> long "datetime"
    <> help "date and time of the transaction. Format: yyyy-mm-dd HH:MM:SS[.ffffff]"
    <> metavar "DATETIME"
    )

notesOpt :: Parser (Maybe String)
notesOpt = optional $
    strOption
    (  short 'n'
    <> long "notes"
    <> help "commentary related to the transaction"
    <> metavar "NOTES"
    )

addOptions :: Parser AddOptions
addOptions = AddOptions <$> amountArg <*> categoryArg <*> dateArg <*> notesOpt

getOptions :: Parser GetOptions
getOptions = GetOptions <$> idArg

updateOptions :: Parser UpdateOptions
updateOptions =
    UpdateOptions <$> idArg <*> amountOpt <*> categoryOpt <*> dateOpt <*> notesOpt

deleteOptions :: Parser DeleteOptions
deleteOptions = DeleteOptions <$> idArg


-- Option handlers

handleAdd :: IConnection conn => conn -> AddOptions -> IO ()
handleAdd conn (AddOptions a c d n) = addTransaction conn a c d n

handleGet :: IConnection conn => conn -> GetOptions -> IO ()
handleGet conn (GetOptions i) = do
    mTrans <- getTransaction conn i
    case mTrans of
        Nothing -> putStrLn $ unwords ["Transaction with id", show i, "does not exist."]
        Just trans -> putStrLn $ showTrans trans

handleUpdate :: IConnection conn => conn -> UpdateOptions -> IO ()
handleUpdate conn (UpdateOptions i a c d n) = updateTransaction conn i a c d n

handleDelete :: IConnection conn => conn -> DeleteOptions -> IO ()
handleDelete conn (DeleteOptions i) = deleteTransaction conn i


-- Command definitions

addCommand :: ParserInfo TransactionOptions
addCommand = info (Add <$> addOptions) (progDesc "Add a new transaction")

getCommand :: ParserInfo TransactionOptions
getCommand = info (Get <$> getOptions) (progDesc "Get the transaction with the given id")

updateCommand :: ParserInfo TransactionOptions
updateCommand =
    info (Update <$> updateOptions) (progDesc "Update the transaction with the given id")

deleteCommand :: ParserInfo TransactionOptions
deleteCommand =
    info (Delete <$> deleteOptions) (progDesc "Delete the transaction with the given id")


-- Transaction command

transactionOptions :: Parser TransactionOptions
transactionOptions = hsubparser $
       command "add" addCommand
    <> command "get" getCommand
    <> command "update" updateCommand
    <> command "delete" deleteCommand

handleTransaction :: IConnection conn => conn -> TransactionOptions -> IO ()
handleTransaction conn (Add args) = handleAdd conn args
handleTransaction conn (Get args) = handleGet conn args
handleTransaction conn (Update args) = handleUpdate conn args
handleTransaction conn (Delete args) = handleDelete conn args