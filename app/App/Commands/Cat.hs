module App.Commands.Cat
  ( cmdCat
  ) where

import App.Commands.Options.Type
import Control.Lens
import Data.Semigroup            ((<>))
import Options.Applicative       hiding (columns)

import qualified App.IO               as IO
import qualified App.Lens             as L
import qualified Data.ByteString.Lazy as LBS

runCat :: CatOptions -> IO ()
runCat opts = do
  let source = opts ^. L.source
  let target = opts ^. L.target

  contents <- IO.readInputFile source

  LBS.writeFile target contents

  return ()

optsCat :: Parser CatOptions
optsCat = CatOptions
  <$> strOption
        (   long "input"
        <>  help "Input file"
        <>  metavar "FILE"
        )
  <*> strOption
        (   long "output"
        <>  help "Output file"
        <>  metavar "FILE"
        )

cmdCat :: Mod CommandFields (IO ())
cmdCat = command "cat"  $ flip info idm $ runCat <$> optsCat
