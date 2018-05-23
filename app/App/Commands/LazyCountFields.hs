{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.LazyCountFields
  ( cmdLazyCountFields
  ) where

import App.Commands.Options.Type
import Control.Lens
import Data.Semigroup                     ((<>))
import HaskellWorks.Data.Sv.Internal.Char
import Options.Applicative                hiding (columns)

import qualified App.Commands.Options.Lens        as L
import qualified App.IO                           as IO
import qualified HaskellWorks.Data.Sv.Lazy.Cursor as SVL

runLazyCountFields :: LazyCountFieldsOptions -> IO ()
runLazyCountFields opts = do
  let filePath  = opts ^. L.filePath
  let delimiter = opts ^. L.delimiter

  !bs <- IO.readInputFile filePath

  let !c = SVL.makeCursor delimiter bs

  putStrLn $ "Number of fields: " <> show (SVL.countFields c)

optsLazyCountFields :: Parser LazyCountFieldsOptions
optsLazyCountFields = LazyCountFieldsOptions
  <$> strOption
        (   long "file"
        <>  help "Separated Value file"
        <>  metavar "STRING"
        )
  <*> option readChar
        (   long "delimiter"
        <>  help "DSV delimiter"
        <>  metavar "CHAR"
        )

cmdLazyCountFields :: Mod CommandFields (IO ())
cmdLazyCountFields = command "lazy-count-fields"  $ flip info idm $ runLazyCountFields <$> optsLazyCountFields
