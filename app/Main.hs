module Main (main) where

import Options.Applicative (execParser)
import Database.HDBC (IConnection (commit, disconnect))
import Database.HDBC.PostgreSQL (connectPostgreSQL)

import CLI.Commands


main :: IO ()
main = do
    cmd <- execParser cli
    conn <- connectPostgreSQL
        "user=obolos password=hidragerum dbname=Obolos host=localhost port=5423"
    handleCli conn cmd
    commit conn
    disconnect conn