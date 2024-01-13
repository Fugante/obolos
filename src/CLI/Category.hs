module CLI.Category
    ( CategoryOptions(..)
    , categoryOptions
    , handleCategory
    ) where

import Data.Maybe (fromJust)
import Database.HDBC (IConnection)
import Options.Applicative

import Models


data AddOptions =
    AddOptions
    { addOptCat :: String
    , addOptScat :: Integer
    }

data GetOptions = GetOptions { getOptId :: Integer }

data UpdateOptions =
    UpdateOptions
    { upOptId :: Integer
    , upOptCat :: Maybe String
    , upOptScat :: Maybe Integer
    }

data DeleteOptions = DeleteOptions { delOptId :: Integer }

data CategoryOptions =
      Add AddOptions
    | Get GetOptions
    | Update UpdateOptions
    | Delete DeleteOptions

categoryArgument :: Parser String
categoryArgument = strArgument (metavar "CATEGORY")

supercategoryArgument :: Parser Integer
supercategoryArgument = argument auto (metavar "SUPERCATECOGORY")

idArgument :: Parser Integer
idArgument = argument auto (metavar "ID")

categoryOption :: Parser (Maybe String)
categoryOption = optional $
    strOption
    (  short 'c'
    <> long "category"
    <> help "name of the category"
    <> metavar "CATEGORY"
    )

supercategoryOption :: Parser (Maybe Integer)
supercategoryOption = optional $
    option
    auto
    (  short 's'
    <> long "supercategory"
    <> help "supercategory index"
    <> metavar "SUPERCATEGORY"
    )

addOptions :: Parser AddOptions
addOptions = AddOptions <$> categoryArgument <*> supercategoryArgument

getOptions :: Parser GetOptions
getOptions = GetOptions <$> idArgument

updateOptions :: Parser UpdateOptions
updateOptions = UpdateOptions <$> idArgument <*> categoryOption <*> supercategoryOption

deleteOptions :: Parser DeleteOptions
deleteOptions = DeleteOptions <$> idArgument

handleAdd :: IConnection conn => conn -> AddOptions -> IO ()
handleAdd conn (AddOptions c sc) = addCategory conn c sc

handleGet :: IConnection conn => conn -> GetOptions -> IO ()
handleGet conn (GetOptions cid) = do
    mCat <- getCategory conn cid
    case mCat of
        Nothing -> putStrLn $ unwords ["Category with id", show cid, "does not exist."]
        Just cat -> putStrLn $ showCategory cat

handleUpdate :: IConnection conn => conn -> UpdateOptions -> IO ()
handleUpdate conn (UpdateOptions cid c sc) = updateCategory conn cid c sc

handleDelete :: IConnection conn => conn -> DeleteOptions -> IO ()
handleDelete conn (DeleteOptions cid) = deleteCategory conn cid

addCommand :: ParserInfo CategoryOptions
addCommand = info (Add <$> addOptions) (progDesc "Add a new category")

getCommand :: ParserInfo CategoryOptions
getCommand = info (Get <$> getOptions) (progDesc "Get the category with given ID")

updateCommand :: ParserInfo CategoryOptions
updateCommand = info
    (Update <$> updateOptions) (progDesc "Update the category with the given ID")

deleteCommand :: ParserInfo CategoryOptions
deleteCommand =
    info (Delete <$> deleteOptions) (progDesc "Delete the category with the given ID")

categoryOptions :: Parser CategoryOptions
categoryOptions = hsubparser $
       command "add" addCommand
    <> command "get" getCommand
    <> command "update" updateCommand
    <> command "delete" deleteCommand

handleCategory :: IConnection conn => conn -> CategoryOptions -> IO ()
handleCategory conn (Add args) = handleAdd conn args
handleCategory conn (Get args) = handleGet conn args
handleCategory conn (Update args) = handleUpdate conn args
handleCategory conn (Delete args) = handleDelete conn args