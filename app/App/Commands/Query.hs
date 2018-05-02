{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Query
  ( cmdQuery
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.List
import Data.Maybe
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Char
import HaskellWorks.Data.Sv.Cursor
import HaskellWorks.Data.Sv.Load
import Options.Applicative                  hiding (columns)

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Builder as B
import qualified System.IO               as IO

repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f a = a:case f a of
  Just b  -> repeatedly f b
  Nothing -> []

runQuery :: Bool -> [Int] -> FilePath -> Char -> IO ()
runQuery createIndex columns filePath delimiter = do
  cursor <- mmapDataFile2 delimiter createIndex filePath

  forM_ (repeatedly nextRow cursor) $ \row -> do
    let fieldCursors = repeatedly nextField row
    let fieldStrings = columnToFieldString fieldCursors <$> columns

    B.hPutBuilder IO.stdout $ mconcat (intersperse "|" fieldStrings) <> B.word8 10

    return ()

  return ()
  where columnToFieldString :: [SvCursor BS.ByteString CsPoppy] -> Int -> B.Builder
        columnToFieldString fields column = maybe mempty (B.byteString . snippet) (drop column fields & listToMaybe)

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
    <*> option readChar
          (   long "delimiter"
          <>  help "DSV delimiter"
          <>  metavar "CHAR"
          )
