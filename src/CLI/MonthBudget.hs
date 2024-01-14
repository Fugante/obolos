module CLI.MonthBudget
    ( MbOptions(..)
    , mbOpts
    , handleMb
    ) where

import Database.HDBC (IConnection)
import Options.Applicative

import Relations.Entities


data AddOptions =
    AddOptions
    { addOptYear :: Integer
    , addOptMonth:: Integer
    , addOptCat :: Integer
    , addOptPlanned :: Integer
    , addOptActual :: Integer
    }

data GetOptions = GetOptions { getOptId :: Integer }

data UpdateOptions =
    UpdateOptions
    { upOptId :: Integer
    , upOptYear :: Maybe Integer
    , upOptMonth :: Maybe Integer
    , upOptCat :: Maybe Integer
    , upOptPlanned :: Maybe Integer
    , upOptActual :: Maybe Integer
    }

data DeleteOptions = DeleteOptions { delOptId :: Integer}

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

getOpts :: Parser GetOptions
getOpts = GetOptions <$> idArg

updateOpts :: Parser UpdateOptions
updateOpts =
    UpdateOptions <$> idArg <*> yearOpt <*> monthOpt <*> catOpt <*> planOpt <*> actOpt

delOpts :: Parser DeleteOptions
delOpts = DeleteOptions <$> idArg


-- Option handlers

handleAdd :: IConnection conn => conn -> AddOptions -> IO ()
handleAdd conn (AddOptions y m c p a) = addMb conn y m c p a

handleGet :: IConnection conn => conn -> GetOptions -> IO ()
handleGet conn (GetOptions i) = do
    mMb <- getMb conn i
    case mMb of
        Nothing -> putStrLn $ unwords ["Month budget with id", show i, "does not exist."]
        Just mb -> putStrLn $ showMb mb

handleUpdate :: IConnection conn => conn -> UpdateOptions -> IO ()
handleUpdate conn (UpdateOptions i y m c p a) = updateMb conn i y m c p a

handleDel :: IConnection conn => conn -> DeleteOptions -> IO ()
handleDel conn (DeleteOptions i) = delMb conn i


-- Command definitions

addCmd :: ParserInfo MbOptions
addCmd = info (Add <$> addOpts) (progDesc "Add a new month budget")

getCmd :: ParserInfo MbOptions
getCmd = info (Get <$> getOpts) (progDesc "Get the month budget with the given id")

updateCmd :: ParserInfo MbOptions
updateCmd =
    info (Update <$> updateOpts) (progDesc "Update the month budget with the given id")

delCmd :: ParserInfo MbOptions
delCmd = info (Delete <$> delOpts) (progDesc "Delete the month budget with the given id")


-- Month Budget command

mbOpts :: Parser MbOptions
mbOpts = hsubparser $
       command "add" addCmd
    <> command "get" getCmd
    <> command "update" updateCmd
    <> command "del" delCmd

handleMb :: IConnection conn => conn -> MbOptions -> IO ()
handleMb conn (Add args) = handleAdd conn args
handleMb conn (Get args) = handleGet conn args
handleMb conn (Update args) = handleUpdate conn args
handleMb conn (Delete args) = handleDel conn args