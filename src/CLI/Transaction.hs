module CLI.Transaction
    ( TransactionOptions(..)
    , tranOpts
    , handleTran
    )
    where

-- Dependencies
import Data.Time (LocalTime)

-- External dependencies
import Data.Aeson (decodeFileStrict)
import Database.HDBC (toSql)
import Options.Applicative

-- Local modules
import qualified DB.Entities.Transaction as T
import DB.Relations
import DB.Views (getAll)


data AddOptions =
      AddOne Integer Integer LocalTime (Maybe String)
    | AddFromJSON FilePath

data GetOptions =
      GetOne Integer
    | GetAll

data UpdateOptions =
      UpdOne Integer (Maybe Integer) (Maybe Integer) (Maybe LocalTime) (Maybe String)
    | UpdFromJSON FilePath

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

fileOpt :: Parser FilePath
fileOpt = strOption
    (  short 'j'
    <> long "from-json"
    <> help "read from json file"
    <> metavar "PATH"
    )

addOneOpt :: Parser AddOptions
addOneOpt = AddOne <$> amtArg <*> catArg <*> dateArg <*> notesOpt

addFromJsonOpt :: Parser AddOptions
addFromJsonOpt = AddFromJSON <$> fileOpt

addOpts :: Parser AddOptions
addOpts = addOneOpt <|> addFromJsonOpt

getOneOpt :: Parser GetOptions
getOneOpt = GetOne <$> idArg

getAllOpt :: Parser GetOptions
getAllOpt = flag' GetAll (short 'a' <> long "all" <> help "Show all transactions")

getOpts :: Parser GetOptions
getOpts = getOneOpt <|> getAllOpt

updOneOpt :: Parser UpdateOptions
updOneOpt = UpdOne <$> idArg <*> amtOpt <*> catOpt <*> dateOpt <*> notesOpt

updFromJsonOpt :: Parser UpdateOptions
updFromJsonOpt = UpdFromJSON <$> fileOpt

updOpts :: Parser UpdateOptions
updOpts = updOneOpt <|> updFromJsonOpt

delOpts :: Parser DeleteOptions
delOpts = DeleteOptions <$> idArg


-- Option handlers

handleAdd :: AddOptions -> IO ()
handleAdd (AddOne a c d n) = addEnts Tran [[toSql a, toSql c, toSql d, toSql n]]
handleAdd (AddFromJSON path) = do
    mTs <- decodeFileStrict path :: IO (Maybe [T.Transaction])
    case mTs of
        Nothing -> putStrLn "Could not parse JSON"
        Just ts -> addEnts Tran . map (tail . T.tranToTuple) $ ts

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
handleUpd (UpdOne i a c d n) =
    updEnts Tran [[toSql i, toSql a, toSql c, toSql d, toSql n]]
handleUpd (UpdFromJSON path) = do
    mTs <- decodeFileStrict path :: IO (Maybe [T.Transaction])
    case mTs of
        Nothing -> putStrLn "Could not parse JSON"
        Just ts -> updEnts Tran $ map T.tranToTuple ts

handleDel :: DeleteOptions -> IO ()
handleDel (DeleteOptions i) = delEnts Tran [[toSql i]]


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