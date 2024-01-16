module CLI.Transaction
    ( TransactionOptions(..)
    , tranOpts
    , handleTran )
    where

import Data.Time ( LocalTime )
import Database.HDBC ( toSql )
import Options.Applicative
    ( Alternative((<|>)),
      optional,
      argument,
      auto,
      command,
      flag',
      help,
      info,
      long,
      metavar,
      option,
      progDesc,
      short,
      strOption,
      hsubparser,
      Parser,
      ParserInfo )

import Relations.Entities
    ( Entity(Tran), addEnt, getEnt, updEnt, delEnt, showEnt )
import Relations.Views ( getAll )


data AddOptions =
    AddOptions
    Integer
    Integer
    LocalTime
    (Maybe String)

data GetOptions =
      GetOne Integer
    | GetAll

data UpdateOptions =
    UpdateOptions
    Integer
    (Maybe Integer)
    (Maybe Integer)
    (Maybe LocalTime)
    (Maybe String)

data DeleteOptions = DeleteOptions Integer

data TransactionOptions =
      Add AddOptions
    | Get GetOptions
    | Update UpdateOptions
    | Delete DeleteOptions


-- Arguments and options

idArg :: Parser Integer
idArg = argument auto (metavar "ID")

amtArg :: Parser Integer
amtArg = argument auto (metavar "AMOUNT" <> help "transaction amount (in cents)")

catArg :: Parser Integer
catArg =
    argument auto (metavar "CATEGORY-ID" <> help "id of the transaction's category")

dateArg :: Parser LocalTime
dateArg =
    argument
    auto
    (  metavar "DATETIME"
    <> help "date and time of the transaction. Format: yyyy-mm-dd HH:MM:SS[.ffffff]"
    )

amtOpt :: Parser (Maybe Integer)
amtOpt = optional $
    option
    auto
    (  short 'a'
    <> long "amount"
    <> help "amount (in cents) of the transaction"
    <> metavar "AMOUNT"
    )

catOpt :: Parser (Maybe Integer)
catOpt = optional $
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

addOpts :: Parser AddOptions
addOpts = AddOptions <$> amtArg <*> catArg <*> dateArg <*> notesOpt

getOneOpt :: Parser GetOptions
getOneOpt = GetOne <$> idArg

getAllOpt :: Parser GetOptions
getAllOpt = flag' GetAll (short 'a' <> long "all" <> help "Show all transactions")

getOpts :: Parser GetOptions
getOpts = getOneOpt <|> getAllOpt

updOpts :: Parser UpdateOptions
updOpts =
    UpdateOptions <$> idArg <*> amtOpt <*> catOpt <*> dateOpt <*> notesOpt

delOpts :: Parser DeleteOptions
delOpts = DeleteOptions <$> idArg


-- Option handlers

handleAdd :: AddOptions -> IO ()
handleAdd (AddOptions a c d n) = addEnt Tran [toSql a, toSql c, toSql d, toSql n]

handleGet :: GetOptions -> IO ()
handleGet (GetOne i) = do
    t <- getEnt Tran i
    case t of
        [i', a, c, d, n] -> do
            putStrLn "id, amount, category id, date, notes"
            putStrLn $ showEnt Tran [i', a, c, d, n]
        _ -> putStrLn $ unwords ["Transaction with id", show i, "does not exist."]
handleGet GetAll = do
    ts <- getAll Tran
    putStrLn "id, amount, category id, date, notes"
    putStrLn . unlines . map (showEnt Tran) $ ts

handleUpd :: UpdateOptions -> IO ()
handleUpd (UpdateOptions i a c d n) =
    updEnt Tran i [toSql i, toSql a, toSql c, toSql d, toSql n]

handleDel :: DeleteOptions -> IO ()
handleDel (DeleteOptions i) = delEnt Tran i


-- Command definitions

addCmd :: ParserInfo TransactionOptions
addCmd = info (Add <$> addOpts) (progDesc "Add a new transaction")

getCmd :: ParserInfo TransactionOptions
getCmd = info (Get <$> getOpts) (progDesc "Get the transaction with the given id")

updCmd :: ParserInfo TransactionOptions
updCmd =
    info (Update <$> updOpts) (progDesc "Update the transaction with the given id")

delCmd :: ParserInfo TransactionOptions
delCmd =
    info (Delete <$> delOpts) (progDesc "Delete the transaction with the given id")


-- Transaction command

tranOpts :: Parser TransactionOptions
tranOpts = hsubparser $
       command "add" addCmd
    <> command "get" getCmd
    <> command "upd" updCmd
    <> command "del" delCmd

handleTran :: TransactionOptions -> IO ()
handleTran (Add args) = handleAdd args
handleTran (Get args) = handleGet args
handleTran (Update args) = handleUpd args
handleTran (Delete args) = handleDel args