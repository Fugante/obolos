module CLI.Commands
    ( CLI
    , cli
    , handleCli
    ) where


import Options.Applicative

import CLI.CRUD as C
import CLI.Info as I


data CLI =
      CRUD C.Crud
    | INFO I.MonthTotals


cmds :: Parser CLI
cmds = CRUD <$> C.crudOpts <|> INFO <$> I.monthTOpts

cli :: ParserInfo CLI
cli = info cmds mempty

handleCli :: CLI -> IO ()
handleCli (CRUD args) = C.handleCrud args
handleCli (INFO args) = I.handleMonthT args