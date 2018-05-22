{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.LazyCountFields
  ( cmdLazyCountFields
  ) where

import App.Commands.Options.Type
import Control.Lens
import Data.Semigroup            ((<>))
import HaskellWorks.Data.Sv.Char
import Options.Applicative       hiding (columns)

import qualified App.Commands.Options.Lens          as L
import qualified Data.ByteString.Lazy               as LBS
import qualified HaskellWorks.Data.Sv.Lazy.Internal as SVL

runLazyCountFields :: LazyCountFieldsOptions -> IO ()
runLazyCountFields opts = do
  let filePath  = opts ^. L.filePath
  let delimiter = opts ^. L.delimiter

  !bs <- LBS.readFile filePath

  let !c = SVL.makeLazyCursor delimiter bs

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
