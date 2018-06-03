module App.Commands where

import App.Commands.Cat
import App.Commands.CreateIndex
import App.Commands.Generate
import App.Commands.Query
import App.Commands.QueryLazy
import Data.Semigroup           ((<>))
import Options.Applicative

commands :: Parser (IO ())
commands = commandsGeneral <|> commandsDebugging

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  -- <>  cmdCreateIndex
  <>  cmdQuery
  <>  cmdQueryLazy

commandsDebugging :: Parser (IO ())
commandsDebugging = subparser $ mempty
  <>  commandGroup "Debugging commands:"
  <>  cmdCat
  <>  cmdGenerate
  <>  hidden
