module App.Commands where

import App.Commands.CreateIndex
import App.Commands.PrettyPrint
import App.Commands.Query
import App.Commands.QueryClassic
import App.Commands.ShowBits
import Data.Semigroup            ((<>))
import Options.Applicative

cmdOpts :: Parser (IO ())
cmdOpts = subparser $ mempty
  <>  cmdCreateIndex
  <>  cmdQueryClassic
  <>  cmdQuery
  <>  cmdShowBits
  <>  cmdPrettyPrint
