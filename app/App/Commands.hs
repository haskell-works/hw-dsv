module App.Commands where

import App.Commands.Cat
import App.Commands.CreateIndex
import App.Commands.Generate
import App.Commands.Query
import App.Commands.QueryLazy
import Data.Semigroup           ((<>))
import Options.Applicative

cmdOpts :: Parser (IO ())
cmdOpts = subparser $ mempty
  <>  cmdCat
  <>  cmdCreateIndex
  <>  cmdGenerate
  <>  cmdQuery
  <>  cmdQueryLazy
