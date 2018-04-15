{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Query
  ( cmdQuery
  ) where

import Control.Applicative
import Data.Monoid
import Data.Word
import Foreign.ForeignPtr
import HaskellWorks.Data.Sv
import Options.Applicative
import System.IO.MMap

import qualified Data.ByteString.Internal as BSI

runQuery :: Bool -> [Int] -> FilePath -> IO ()
runQuery createIndex _column filePath = do
  cursor <- mmapDataFile createIndex filePath

  return ()

cmdQuery :: Mod CommandFields (IO ())
cmdQuery = command "query"  $ flip info idm $ runQuery
    <$> switch (long "create-index")
    <*> many
        ( option auto
          (   long "column"
          <>  short 'k'
          <>  help "Column to select"
          <>  metavar "COLUMN INDEX" ))
    <*> strOption
          (   long "file"
          <>  help "Separated Value file"
          <>  metavar "STRING"
          )
