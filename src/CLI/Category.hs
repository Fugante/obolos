module CLI.Category
    ( CategoryOptions(..)
    , catOpts
    , handleCat
    )
    where

-- External dependencies
import Data.Aeson (decodeFileStrict, encode)
import qualified Data.ByteString.Lazy as B
import Database.HDBC (toSql)
import Options.Applicative

-- Local modules
import qualified DB.Entities.Category as C
import DB.Relations
import DB.Views (getAll)


data Cardinality =
      One Integer
    | All

data Format =
      CSV
    | JSON

data AddOptions =
      AddOne String Integer
    | AddFromJSON FilePath

data GetOptions = GetOptions Cardinality Format

data UpdateOptions =
      UpdOne Integer (Maybe String) (Maybe Integer)
    | UpdFromJSON FilePath

data DeleteOptions = DeleteOptions [Integer]

data CategoryOptions =
      Add AddOptions
    | Get GetOptions
    | Update UpdateOptions
    | Delete DeleteOptions


-- Arguments and options

catArg :: Parser String
catArg = strArgument (metavar "CATEGORY")

sCatArg :: Parser Integer
sCatArg = argument auto (metavar "SUPERCATECOGORY")

idArg :: Parser Integer
idArg = argument auto (metavar "ID")

catOpt :: Parser (Maybe String)
catOpt = optional $
    strOption
    (  short 'c'
    <> long "category"
    <> help "name of the category"
    <> metavar "CATEGORY"
    )

sCatOpt :: Parser (Maybe Integer)
sCatOpt = optional $
    option
    auto
    (  short 's'
    <> long "supercategory"
    <> help "supercategory index"
    <> metavar "SUPERCATEGORY"
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
addOneOpt = AddOne <$> catArg <*> sCatArg

addFromJsonOpt :: Parser AddOptions
addFromJsonOpt = AddFromJSON <$> fileOpt

addOpts :: Parser AddOptions
addOpts = addOneOpt <|> addFromJsonOpt

getOpts :: Parser GetOptions
getOpts = GetOptions <$> carOpt <*> formatOpt

updOneOpt :: Parser UpdateOptions
updOneOpt = UpdOne <$> idArg <*> catOpt <*> sCatOpt

updFromJsonOpt :: Parser UpdateOptions
updFromJsonOpt = UpdFromJSON <$> fileOpt

updOpts :: Parser UpdateOptions
updOpts = updOneOpt <|> updFromJsonOpt

delOpts :: Parser DeleteOptions
delOpts = DeleteOptions <$> some idArg


-- Option handlers

handleAdd :: AddOptions -> IO ()
handleAdd (AddOne c sc) = addEnts Cat [[toSql c, toSql sc]]
handleAdd (AddFromJSON path) = do
    mCs <- decodeFileStrict path :: IO (Maybe [C.Category])
    case mCs of
        Nothing -> putStrLn "Could not parse JSON"
        Just cs -> addEnts Cat . map (tail . C.catToTuple) $ cs

handleGet :: GetOptions -> IO ()
handleGet (GetOptions card form) = do
    ts <- case card of
        One i -> (:[]) <$> getEnt Cat i
        All -> getAll Cat
    case form of
        CSV -> putStrLn $
            "id,category,supercategory\n" ++ unlines (map (showEnt Cat) ts)
        JSON -> do
            B.putStr $ encode (map C.tupleToCat ts)
            putStrLn ""

handleUpd :: UpdateOptions -> IO ()
handleUpd (UpdOne i c s) = updEnts Cat [[toSql i, toSql c, toSql s]]
handleUpd (UpdFromJSON path) = do
    mCs <- decodeFileStrict path :: IO (Maybe [C.Category])
    case mCs of
        Nothing -> putStrLn "Could not parse JSON"
        Just cs -> updEnts Cat $ map C.catToTuple cs

handleDel :: DeleteOptions -> IO ()
handleDel (DeleteOptions is) = delEnts Cat $ map ((:[]) . toSql) is


-- Command definitions

addCmd :: ParserInfo CategoryOptions
addCmd = info (Add <$> addOpts) (progDesc "Add new categories")

getCmd :: ParserInfo CategoryOptions
getCmd = info (Get <$> getOpts) (progDesc "Get categories")

updCmd :: ParserInfo CategoryOptions
updCmd = info
    (Update <$> updOpts) (progDesc "Update cateogories")

delCmd :: ParserInfo CategoryOptions
delCmd =
    info (Delete <$> delOpts) (progDesc "Delete categories")


-- Category command

catOpts :: Parser CategoryOptions
catOpts = hsubparser $
       command "add" addCmd
    <> command "get" getCmd
    <> command "upd" updCmd
    <> command "del" delCmd

handleCat :: CategoryOptions -> IO ()
handleCat (Add args) = handleAdd args
handleCat (Get args) = handleGet args
handleCat (Update args) = handleUpd args
handleCat (Delete args) = handleDel args