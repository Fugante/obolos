module CLI.Category
    ( CategoryOptions(..)
    , categoryOptions
    , handleCategory
    ) where

import Database.HDBC (IConnection)
import Options.Applicative

import Models


data AddOptions =
    AddOptions
    { addCat :: String
    , addScat :: Integer
    }

data GetOptions = GetOptions { getId :: Integer }

data UpdateOptions =
    UpdateOptions
    { upId :: Integer
    , upCat :: String
    , upScat :: Integer
    }

data DeleteOptions = DeleteOptions { delId :: Integer }

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

categoryOption :: Parser String
categoryOption =
    strOption
    (  short 'c'
    <> long "category"
    <> help "name of the category"
    <> metavar "CATEGORY"
    <> value ""
    )

supercategoryOption :: Parser Integer
supercategoryOption =
    option
    auto
    (  short 's'
    <> long "supercategory"
    <> help "supercategory index"
    <> metavar "SUPERCATEGORY"
    <> value 1
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
handleAdd conn (AddOptions c sc) = addCategory conn (Category 0 c sc)

handleGet :: IConnection conn => conn -> GetOptions -> IO ()
handleGet conn (GetOptions cid) = do
    mCat <- getCategory conn cid
    case mCat of
        Nothing -> do
            putStrLn $ unwords ["Category with id", show cid, "does not exist."]
        Just cat -> print cat

handleUpdate :: IConnection conn => conn -> UpdateOptions -> IO ()
handleUpdate conn (UpdateOptions cid c sc) = do
    mCat <- getCategory conn cid
    case mCat of
        Nothing -> do
            putStrLn $ unwords ["Category with id", show cid, "does not exist."]
        Just (Category _ c' sc') -> do
            case (null c, sc == 1) of
                (False, False) -> updateCategory conn (Category cid c sc)
                (False, True) -> updateCategory conn (Category cid c sc')
                (True, False) -> updateCategory conn (Category cid c' sc)
                _ -> return ()

handleDelete :: IConnection conn => conn -> DeleteOptions -> IO ()
handleDelete conn (DeleteOptions cid) = deleteCategory conn cid

addCommand :: ParserInfo CategoryOptions
addCommand = info (Add <$> addOptions) (progDesc "Add a new category.")

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