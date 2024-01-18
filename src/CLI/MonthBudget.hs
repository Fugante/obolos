module CLI.MonthBudget
    ( MbOptions(..)
    , mbOpts
    , handleMb )
    where

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
      hsubparser,
      Parser,
      ParserInfo
      )

import Relations.Entities
    ( Entity(Mb), addEnts, getEnt, updEnt, delEnts, showEnt )
import Relations.Views ( getAll )


data AddOptions =
    AddOptions
    Integer
    Integer
    Integer
    Integer
    Integer

data GetOptions =
      GetOne Integer
    | GetAll

data UpdateOptions =
    UpdateOptions
    Integer
    (Maybe Integer)
    (Maybe Integer)
    (Maybe Integer)
    (Maybe Integer)
    (Maybe Integer)

data DeleteOptions = DeleteOptions Integer

data MbOptions =
      Add AddOptions
    | Get GetOptions
    | Update UpdateOptions
    | Delete DeleteOptions


-- Arguments and options

idArg :: Parser Integer
idArg = argument auto (metavar "ID")

yearArg :: Parser Integer
yearArg = argument auto (metavar "YEAR" <> help "Year of the budget")

monthArg :: Parser Integer
monthArg = argument auto (metavar "MONTH" <> help "Month of the budget")

catArg :: Parser Integer
catArg = argument auto (metavar "CATEGORY-ID" <> help "id of the budget's category")

planArg :: Parser Integer
planArg = argument auto (metavar "PLANNED" <> help "Planned amount for the month")

actArg :: Parser Integer
actArg =
    argument auto (metavar "ACTUAL" <> help "Actual amoung spent/gained in the month")

yearOpt :: Parser (Maybe Integer)
yearOpt = optional $
    option
    auto
    (  short 'y'
    <> long "year"
    <> help "Year of the budget"
    <> metavar "YEAR"
    )

monthOpt :: Parser (Maybe Integer)
monthOpt = optional $
    option
    auto
    (  short 'm'
    <> long "month"
    <> help "Month of the budget"
    <> metavar "YEAR"
    )

catOpt :: Parser (Maybe Integer)
catOpt = optional $
    option
    auto
    (  short 'c'
    <> long "category"
    <> help "id of the budget's category"
    <> metavar "CATEGORY-ID"
    )

planOpt :: Parser (Maybe Integer)
planOpt = optional $
    option
    auto
    (  short 'p'
    <> long "planned"
    <> metavar "PLANNED"
    <> help "Planned amount for the month"
    )

actOpt :: Parser (Maybe Integer)
actOpt = optional $
    option
    auto
    (  short 'a'
    <> long "actual"
    <> metavar "ACTUAL"
    <> help "Actual amoung spent/gained in the month"
    )

addOpts :: Parser AddOptions
addOpts =
    AddOptions <$> yearArg <*> monthArg <*> catArg <*> planArg <*> actArg

getOneOpt :: Parser GetOptions
getOneOpt = GetOne <$> idArg

getAllOpt :: Parser GetOptions
getAllOpt = flag' GetAll (short 'a' <> long "all" <> help "Show all month budgets")

getOpts :: Parser GetOptions
getOpts = getOneOpt <|> getAllOpt

updateOpts :: Parser UpdateOptions
updateOpts =
    UpdateOptions <$> idArg <*> yearOpt <*> monthOpt <*> catOpt <*> planOpt <*> actOpt

delOpts :: Parser DeleteOptions
delOpts = DeleteOptions <$> idArg


-- Option handlers

handleAdd :: AddOptions -> IO ()
handleAdd (AddOptions y m c p a) =
    addEnts Mb [[toSql y, toSql m, toSql c, toSql p, toSql a]]

handleGet :: GetOptions -> IO ()
handleGet (GetOne i) = do
    t <- getEnt Mb i
    case t of
        [i', y, m, c, p, a] -> do
            putStrLn "id, year, month, category id, planned, actual"
            putStrLn $ showEnt Mb [i', y, m, c, p, a]
        _ -> putStrLn $ unwords ["Month budget with id", show i, "does not exist."]
handleGet GetAll = do
    ts <- getAll Mb
    putStrLn "id, year, month, category id, planned, actual"
    putStrLn . unlines . map (showEnt Mb) $ ts

handleUpd :: UpdateOptions -> IO ()
handleUpd (UpdateOptions i y m c p a) =
    updEnt Mb i [toSql i, toSql y, toSql m, toSql c, toSql p, toSql a]

handleDel :: DeleteOptions -> IO ()
handleDel (DeleteOptions i) = delEnts Mb [[toSql i]]


-- Command definitions

addCmd :: ParserInfo MbOptions
addCmd = info (Add <$> addOpts) (progDesc "Add a new month budget")

getCmd :: ParserInfo MbOptions
getCmd = info (Get <$> getOpts) (progDesc "Get the month budget with the given id")

updCmd :: ParserInfo MbOptions
updCmd =
    info (Update <$> updateOpts) (progDesc "Update the month budget with the given id")

delCmd :: ParserInfo MbOptions
delCmd = info (Delete <$> delOpts) (progDesc "Delete the month budget with the given id")


-- Month Budget command

mbOpts :: Parser MbOptions
mbOpts = hsubparser $
       command "add" addCmd
    <> command "get" getCmd
    <> command "upd" updCmd
    <> command "del" delCmd

handleMb :: MbOptions -> IO ()
handleMb (Add args) = handleAdd args
handleMb (Get args) = handleGet args
handleMb (Update args) = handleUpd args
handleMb (Delete args) = handleDel args