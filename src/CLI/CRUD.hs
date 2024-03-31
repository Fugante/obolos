module CLI.CRUD
  ( Crud
  , crudOpts
  , crud
  , handleCrud
  ) where


-- External dependencies
import Data.Aeson (decodeFileStrict, encode)
import Options.Applicative

-- Local modules
import qualified DB.Entities.Category as C
import qualified DB.Entities.Transaction as T
import qualified DB.Entities.MonthBudget as MB
import qualified DB.Entities.Entity as E
import qualified Data.ByteString.Lazy as B


data GetOpts = One Integer | All

data Cmd =
      Add FilePath
    | Upd FilePath
    | Del [Integer]
    | Get GetOpts

data Ent = Cat | Tran | Mb

data Crud = Crud Cmd Ent


addCmd :: ParserInfo Cmd
addCmd = info (Add <$> strOption (short 'f' <> long "file" <> metavar "FILE")) mempty

updCmd :: ParserInfo Cmd
updCmd = info (Upd <$> strOption (short 'f' <> long "file" <> metavar "FILE")) mempty

delCmd :: ParserInfo Cmd
delCmd = info (Del <$> many (argument auto (metavar "ID"))) mempty

getCmd :: ParserInfo Cmd
getCmd =
    info
    (Get <$>
        (   One <$> argument auto (metavar "ID")
        <|> flag' All (short 'a' <> long "all")
        )
    )
    mempty

cmds :: Parser Cmd
cmds = hsubparser $
       command "add" addCmd
    <> command "upd" updCmd
    <> command "del" delCmd
    <> command "get" getCmd

entOpts :: Parser Ent
entOpts = flag' Cat (long "cat") <|> flag' Tran (long "tran") <|> flag' Mb (long "mb")

crudOpts :: Parser Crud
crudOpts = Crud <$> cmds <*> entOpts

crud :: ParserInfo Crud
crud = info crudOpts mempty

insertEntity :: E.Entity e => Maybe [e] -> IO ()
insertEntity mEs = case mEs of
    Nothing -> putStrLn "Could not parse JSON"
    Just es -> E.insert es

updateEntity :: E.Entity e => Maybe [e] -> IO ()
updateEntity mEs = case mEs of
    Nothing -> putStrLn "Could not parse JSON"
    Just es -> E.update es

handleCrud :: Crud -> IO ()
handleCrud (Crud (Add path) Cat) = do
    mEs <- decodeFileStrict path :: IO (Maybe [C.Category])
    insertEntity mEs
handleCrud (Crud (Add path) Tran) = do
    mEs <- decodeFileStrict path :: IO (Maybe [T.Transaction])
    insertEntity mEs
handleCrud (Crud (Add path) Mb) = do
    mEs <- decodeFileStrict path :: IO (Maybe [MB.MonthBudget])
    insertEntity mEs
handleCrud (Crud (Upd path) Cat) = do
    mEs <- decodeFileStrict path :: IO (Maybe [C.Category])
    updateEntity mEs
handleCrud (Crud (Upd path) Tran) = do
    mEs <- decodeFileStrict path :: IO (Maybe [T.Transaction])
    updateEntity mEs
handleCrud (Crud (Upd path) Mb) = do
    mEs <- decodeFileStrict path :: IO (Maybe [MB.MonthBudget])
    updateEntity mEs
handleCrud (Crud (Del ids) ent) = do
    case ent of
        Cat -> E.delete (fmap (\i -> E.empty {C.id = Just i}) ids :: [C.Category])
        Tran -> E.delete (fmap (\i -> E.empty {T.id = Just i}) ids :: [T.Transaction])
        Mb -> E.delete (fmap (\i -> E.empty {MB.id = Just i}) ids :: [MB.MonthBudget])
handleCrud (Crud (Get (One i)) ent) = do
    bStr <- case ent of
        Cat -> fmap encode (E.get i :: IO C.Category)
        Tran -> fmap encode (E.get i :: IO T.Transaction)
        Mb -> fmap encode (E.get i :: IO MB.MonthBudget)
    B.putStr bStr
    putStrLn ""
handleCrud (Crud (Get All) ent) = do
    bStr <- case ent of
        Cat -> fmap encode (E.getAll :: IO [C.Category])
        Tran -> fmap encode (E.getAll :: IO [T.Transaction])
        Mb -> fmap encode (E.getAll :: IO [MB.MonthBudget])
    B.putStr bStr
    putStrLn ""