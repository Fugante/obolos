module Main (main) where

import Options.Applicative (execParser)
import Database.HDBC (IConnection (commit, disconnect))
import Database.HDBC.PostgreSQL (connectPostgreSQL)

import CLI.Commands
import Settings


main :: IO ()
main = do
    mSettings <- getSettings
    case mSettings of
        Nothing -> putStrLn "No settings found"
        Just settings -> do
            cmd <- execParser cli
            conn <- connectPostgreSQL . connString . db $ settings
            handleCli conn cmd
            commit conn
            disconnect conn