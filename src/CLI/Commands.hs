module CLI.Commands
    ( cli
    , handleCli
    ) where

import Database.HDBC (IConnection)
import Options.Applicative

import CLI.Category
import CLI.Transaction
import CLI.MonthBudget


data CLI =
      Category CategoryOptions
    | Transaction TransactionOptions
    | MonthBudget MbOptions

catCmd :: ParserInfo CLI
catCmd =
    info (Category <$> categoryOptions) (fullDesc <> progDesc "Handle categories")

transCmd :: ParserInfo CLI
transCmd =
    info (Transaction <$> transactionOptions) (fullDesc <> progDesc "Handle transactions")

mbCmd :: ParserInfo CLI
mbCmd = info (MonthBudget <$> mbOpts) (fullDesc <> progDesc "Handle month budgets")

obolosOptions :: Parser CLI
obolosOptions = hsubparser $
       command "category" catCmd
    <> command "transaction" transCmd
    <> command "mb" mbCmd

cli :: ParserInfo CLI
cli =
    info (obolosOptions <**> helper) (fullDesc <> progDesc "You better pay the ferryman")

handleCli :: IConnection conn => conn -> CLI -> IO ()
handleCli conn (Category args) = handleCategory conn args
handleCli conn (Transaction args) = handleTransaction conn args
handleCli conn (MonthBudget args) = handleMb conn args