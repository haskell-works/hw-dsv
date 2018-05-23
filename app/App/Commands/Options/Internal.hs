module App.Commands.Options.Internal
  ( IndexMethod(..)
  ) where

data IndexMethod
  = Lazy
  | Strict
  | Classic
  deriving (Eq, Read, Show)
