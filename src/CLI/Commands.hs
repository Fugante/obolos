module CLI.Commands
    ( cli
    , handleCli )
    where

import Options.Applicative
    ( (<**>),
      command,
      fullDesc,
      info,
      progDesc,
      helper,
      hsubparser,
      Parser,
      ParserInfo )

import CLI.Category ( CategoryOptions, catOpts, handleCat )
import CLI.Transaction ( TransactionOptions, tranOpts, handleTran )
import CLI.MonthBudget ( MbOptions, mbOpts, handleMb )


data CLI =
      Category CategoryOptions
    | Transaction TransactionOptions
    | MonthBudget MbOptions

catCmd :: ParserInfo CLI
catCmd =
    info (Category <$> catOpts) (fullDesc <> progDesc "Handle categories")

transCmd :: ParserInfo CLI
transCmd =
    info (Transaction <$> tranOpts) (fullDesc <> progDesc "Handle transactions")

mbCmd :: ParserInfo CLI
mbCmd = info (MonthBudget <$> mbOpts) (fullDesc <> progDesc "Handle month budgets")

obolosOptions :: Parser CLI
obolosOptions = hsubparser $
       command "cat" catCmd
    <> command "tran" transCmd
    <> command "mb" mbCmd

cli :: ParserInfo CLI
cli =
    info (obolosOptions <**> helper) (fullDesc <> progDesc "You better pay the ferryman")

handleCli :: CLI -> IO ()
handleCli (Category args) = handleCat args
handleCli (Transaction args) = handleTran args
handleCli (MonthBudget args) = handleMb args