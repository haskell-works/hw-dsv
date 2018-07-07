module App.Commands.Cat
  ( cmdCat
  ) where

import App.Commands.Options.Type
import Control.Lens
import Data.Semigroup            ((<>))
import Options.Applicative

import qualified App.IO   as IO
import qualified App.Lens as L

runCat :: CatOptions -> IO ()
runCat opts = do
  let source = opts ^. L.source
  let target = opts ^. L.target

  contents <- IO.readInputFile source

  IO.writeOutputFile target contents

  return ()

optsCat :: Parser CatOptions
optsCat = CatOptions
  <$> strOption
        (   long "input"
        <>  help "Input file"
        <>  metavar "FILE"
        <>  showDefault
        <>  value "-"
        )
  <*> strOption
        (   long "output"
        <>  help "Output file"
        <>  metavar "FILE"
        <>  showDefault
        <>  value "-"
        )

cmdCat :: Mod CommandFields (IO ())
cmdCat = command "cat"  $ flip info idm $ runCat <$> optsCat
