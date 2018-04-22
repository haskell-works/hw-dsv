module App.Commands where

import App.Commands.CreateIndex
import App.Commands.Query
import Options.Applicative

cmdOpts :: Parser (IO ())
cmdOpts = subparser $ mempty
  <>  cmdCreateIndex
  <>  cmdQuery
