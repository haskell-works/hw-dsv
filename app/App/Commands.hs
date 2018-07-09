module App.Commands where

import App.Commands.Cat
import App.Commands.Generate
import App.Commands.IndexWord8s
import App.Commands.QueryLazy
import App.Commands.QueryStrict
import Data.Semigroup           ((<>))
import Options.Applicative

commands :: Parser (IO ())
commands = commandsGeneral <|> commandsDebugging

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  -- <>  cmdCreateIndex
  <>  cmdQueryLazy
  <>  cmdQueryStrict

commandsDebugging :: Parser (IO ())
commandsDebugging = subparser $ mempty
  <>  commandGroup "Debugging commands:"
  <>  cmdCat
  <>  cmdIndexWord8s
  <>  cmdGenerate
  <>  hidden
