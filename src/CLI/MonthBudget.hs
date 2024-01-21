module CLI.MonthBudget
    ( MbOptions(..)
    , mbOpts
    , handleMb
    )
    where

-- External dependencies
import Data.Aeson (decodeFileStrict, encode)
import Database.HDBC (toSql)
import Options.Applicative

-- Local modules
import qualified DB.Entities.MonthBudget as Mb
import DB.Relations
    ( Entity(Mb), addEnts, getEnt, updEnts, delEnts, showEnt )
import DB.Views (getAll)
import qualified Data.ByteString.Lazy as B


data Cardinality =
      One Integer
    | All

data Format =
      CSV
    | JSON

data AddOptions =
      AddOne Integer Integer Integer Integer Integer
    | AddFromJSON FilePath

data GetOptions = GetOptions Cardinality Format

data UpdateOptions =
      UpdOne
        Integer
        (Maybe Integer)
        (Maybe Integer)
        (Maybe Integer)
        (Maybe Integer)
        (Maybe Integer)
    | UpdFromJSON FilePath

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

fileOpt :: Parser FilePath
fileOpt = strOption
    (  short 'j'
    <> long "from-json"
    <> help "read from json file"
    <> metavar "PATH"
    )

oneOpt :: Parser Cardinality
oneOpt = One <$> idArg

allOpt :: Parser Cardinality
allOpt = flag' All
    (  short 'a'
    <> long "all"
    <> help "show all categories"
    )

carOpt :: Parser Cardinality
carOpt = oneOpt <|> allOpt

csvOpt :: Parser Format
csvOpt = flag CSV CSV
    (  short 'c'
    <> long "csv"
    <> help "show output in csv format"
    )

jsonOpt :: Parser Format
jsonOpt = flag' JSON
    (  short 'j'
    <> long "json"
    <> help "show output in json format"
    )

formatOpt :: Parser Format
formatOpt = csvOpt <|> jsonOpt

addOneOpt :: Parser AddOptions
addOneOpt = AddOne <$> yearArg <*> monthArg <*> catArg <*> planArg <*> actArg

addFromJsonOpt :: Parser AddOptions
addFromJsonOpt = AddFromJSON <$> fileOpt

addOpts :: Parser AddOptions
addOpts = addOneOpt <|> addFromJsonOpt

getOpts :: Parser GetOptions
getOpts = GetOptions <$> carOpt <*> formatOpt

updOneOpt :: Parser UpdateOptions
updOneOpt = UpdOne <$> idArg <*> yearOpt <*> monthOpt <*> catOpt <*> planOpt <*> actOpt

updFromJsonOpt :: Parser UpdateOptions
updFromJsonOpt = UpdFromJSON <$> fileOpt

updateOpts :: Parser UpdateOptions
updateOpts = updOneOpt <|> updFromJsonOpt

delOpts :: Parser DeleteOptions
delOpts = DeleteOptions <$> idArg


-- Option handlers

handleAdd :: AddOptions -> IO ()
handleAdd (AddOne y m c p a) = addEnts Mb [[toSql y, toSql m, toSql c, toSql p, toSql a]]
handleAdd (AddFromJSON path) = do
    mMbs <- decodeFileStrict path :: IO (Maybe [Mb.MonthBudget])
    case mMbs of
        Nothing -> putStrLn "Could not parse JSON"
        Just mbs -> addEnts Mb . map (tail . Mb.mbToTuple) $ mbs

handleGet :: GetOptions -> IO ()
handleGet (GetOptions card form) = do
    mbs <- case card of
        One i -> (:[]) <$> getEnt Mb i
        All -> getAll Mb
    case form of
        CSV -> putStrLn $
            "id,year,month,categoryId,planned,actual\n" ++ unlines (map (showEnt Mb) mbs)
        JSON -> do
            B.putStr $ encode (map Mb.tupleToMb mbs)
            putStrLn ""

handleUpd :: UpdateOptions -> IO ()
handleUpd (UpdOne i y m c p a) =
    updEnts Mb [[toSql i, toSql y, toSql m, toSql c, toSql p, toSql a]]
handleUpd (UpdFromJSON path) = do
    mMbs <- decodeFileStrict path :: IO (Maybe [Mb.MonthBudget])
    case mMbs of
        Nothing -> putStrLn "Could not parse JSON"
        Just mbs -> updEnts Mb $ map Mb.mbToTuple mbs

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