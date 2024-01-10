module Main (main) where

import Database.HDBC (IConnection (commit, disconnect))
import Database.HDBC.PostgreSQL (connectPostgreSQL)
import Options.Applicative

import CLI.Category


categoryCommand :: ParserInfo CategoryOptions
categoryCommand =
    info (categoryOptions <**> helper) (fullDesc <> progDesc "Handle categories")

main :: IO ()
main = do
    conn <- connectPostgreSQL
        "user=obolos password=hidragerum dbname=Obolos host=localhost port=5423"
    cmd <- execParser categoryCommand
    handleCategory conn cmd
    commit conn
    disconnect conn