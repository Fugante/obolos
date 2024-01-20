module CLI.Category
    ( CategoryOptions(..)
    , catOpts
    , handleCat
    )
    where

import Database.HDBC ( toSql )
import Options.Applicative
    ( Alternative((<|>))
    , optional
    , argument
    , auto
    , command
    , flag'
    , help
    , info
    , long
    , metavar
    , option
    , progDesc
    , short
    , strArgument
    , strOption
    , hsubparser
    , Parser
    , ParserInfo
    , some
    )

import qualified DB.Entities.Category as C
import DB.Relations
    ( Entity(Cat)
    , addEnts
    , getEnt
    , updEnt
    , delEnts
    , showEnt
    )
import DB.Views ( getAll )
import Data.Aeson ( decodeFileStrict )


data AddOptions =
      AddOne String Integer
    | FromJson FilePath

data GetOptions =
      GetOne Integer
    | GetAll

data UpdateOptions =
    UpdateOptions
    Integer
    (Maybe String)
    (Maybe Integer)

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

addOneOpt :: Parser AddOptions
addOneOpt = AddOne <$> catArg <*> sCatArg

fromJsonOpt :: Parser AddOptions
fromJsonOpt = FromJson <$> fileOpt

addOpts :: Parser AddOptions
addOpts = addOneOpt <|> fromJsonOpt

getOneOpt :: Parser GetOptions
getOneOpt = GetOne <$> idArg

getAllOpt :: Parser GetOptions
getAllOpt = flag' GetAll (short 'a' <> long "all" <> help "Show all categories")

getOpts :: Parser GetOptions
getOpts = getOneOpt <|> getAllOpt

updOpts :: Parser UpdateOptions
updOpts = UpdateOptions <$> idArg <*> catOpt <*> sCatOpt

delOpts :: Parser DeleteOptions
delOpts = DeleteOptions <$> some idArg


-- Option handlers

handleAdd :: AddOptions -> IO ()
handleAdd (AddOne c sc) = addEnts Cat [[toSql c, toSql sc]]
handleAdd (FromJson path) = do
    mCs <- decodeFileStrict path :: IO (Maybe [C.Category])
    case mCs of
        Nothing -> putStrLn "Could not parse JSON"
        Just cs -> addEnts Cat . map (tail . C.catToTuple) $ cs

handleGet :: GetOptions -> IO ()
handleGet (GetOne i) = do
    t <- getEnt Cat i
    case t of
        [i', c, s] -> do
            putStrLn "id, category, super category"
            putStrLn $ showEnt Cat [i', c, s]
        _ -> putStrLn $ "Category with id " ++ show i ++ " does not exist."
handleGet GetAll = do
    ts <- getAll Cat
    putStrLn "id, category, super category"
    putStrLn . unlines . map (showEnt Cat) $ ts

handleUpd :: UpdateOptions -> IO ()
handleUpd (UpdateOptions i c s) = updEnt Cat i [toSql i, toSql c, toSql s]

handleDel :: DeleteOptions -> IO ()
handleDel (DeleteOptions is) = delEnts Cat $ map ((:[]) . toSql) is


-- Command definitions

addCmd :: ParserInfo CategoryOptions
addCmd = info (Add <$> addOpts) (progDesc "Add a new category")

getCmd :: ParserInfo CategoryOptions
getCmd = info (Get <$> getOpts) (progDesc "Get the category with given ID")

updCmd :: ParserInfo CategoryOptions
updCmd = info
    (Update <$> updOpts) (progDesc "Update the category with the given ID")

delCmd :: ParserInfo CategoryOptions
delCmd =
    info (Delete <$> delOpts) (progDesc "Delete the category with the given ID")


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