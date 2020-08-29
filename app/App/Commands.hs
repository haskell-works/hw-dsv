module App.Commands where

import App.Commands.CreateIndex
import App.Commands.Generate
import App.Commands.IndexWord8s
import App.Commands.QueryLazy
import App.Commands.QueryStrict
import App.Commands.RangeJoin
import Options.Applicative

{- HLINT ignore "Monoid law, left identity" -}

commands :: Parser (IO ())
commands = commandsGeneral <|> commandsDebugging

commandsGeneral :: Parser (IO ())
commandsGeneral = subparser $ mempty
  <>  commandGroup "Commands:"
  <>  cmdCreateIndex
  <>  cmdQueryLazy
  <>  cmdQueryStrict
  <>  cmdRangeJoin

commandsDebugging :: Parser (IO ())
commandsDebugging = subparser $ mempty
  <>  commandGroup "Debugging commands:"
  <>  cmdIndexWord8s
  <>  cmdGenerate
  <>  hidden
