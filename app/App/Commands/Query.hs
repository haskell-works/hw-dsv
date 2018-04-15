module App.Commands.Query
  ( cmdQuery
  ) where

import Control.Applicative
import Data.Monoid
import Options.Applicative

runQuery :: Bool -> [Int] -> FilePath -> IO ()
runQuery _createIndex _column _filePath = do
  -- toInterestBitsVector
  return ()
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
