module Main (main) where

import Database.HDBC ( IConnection (disconnect) )
import Database.HDBC.PostgreSQL ( connectPostgreSQL )
import Options.Applicative ( execParser )

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
            handleCli cmd
            disconnect conn