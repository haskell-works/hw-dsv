module App.Commands.Cat
  ( cmdCat
  ) where

import App.Commands.Options.Type
import Control.Lens
import Data.Semigroup            ((<>))
import Options.Applicative       hiding (columns)

import qualified App.Commands.Options.Lens as L
import qualified App.IO                    as IO
import qualified Data.ByteString.Lazy      as LBS

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
        (   long "source"
        <>  help "Source file"
        <>  metavar "INT"
        )
  <*> strOption
        (   long "target"
        <>  help "Target"
        <>  metavar "INT"
        )

cmdCat :: Mod CommandFields (IO ())
cmdCat = command "cat"  $ flip info idm $ runCat <$> optsCat
