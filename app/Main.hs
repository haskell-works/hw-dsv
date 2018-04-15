module Main where

import App.Commands
import Control.Monad
import Options.Applicative

main :: IO ()
main = join $ execParser (info cmdOpts idm)
