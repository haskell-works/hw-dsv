module App.Commands where

import App.Commands.Cat
import App.Commands.CreateFastIndex
import App.Commands.CreateIndex
import App.Commands.CreateLazyIndex
import App.Commands.Generate
import App.Commands.LazyCountFields
import App.Commands.PrettyPrint
import App.Commands.Query
import App.Commands.QueryClassic
import App.Commands.QueryLazy
import App.Commands.QueryLazy9
import App.Commands.ShowBits
import Data.Semigroup               ((<>))
import Options.Applicative

cmdOpts :: Parser (IO ())
cmdOpts = subparser $ mempty
  <>  cmdCat
  <>  cmdCreateFastIndex
  <>  cmdCreateIndex
  <>  cmdCreateLazyIndex
  <>  cmdGenerate
  <>  cmdLazyCountFields
  <>  cmdPrettyPrint
  <>  cmdQuery
  <>  cmdQueryClassic
  <>  cmdQueryLazy
  <>  cmdQueryLazy9
  <>  cmdShowBits
