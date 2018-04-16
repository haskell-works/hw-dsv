{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Query
  ( cmdQuery
  ) where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Monoid
import HaskellWorks.Data.Sv
import Options.Applicative  hiding (columns)

import qualified Data.ByteString.Char8 as C8

repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f a = a:case f a of
  Just b  -> repeatedly f b
  Nothing -> []

runQuery :: Bool -> [Int] -> FilePath -> IO ()
runQuery createIndex columns filePath = do
  cursor <- mmapDataFile createIndex filePath

  forM_ (repeatedly nextRow cursor) $ \row -> do
    let fields = repeatedly nextField row
    let columnToFieldString column = maybe "" (C8.unpack . snippet) (drop column fields & listToMaybe)
    let fieldStrings = columnToFieldString <$> columns
    putStrLn $ mconcat $ intersperse "|" fieldStrings

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
