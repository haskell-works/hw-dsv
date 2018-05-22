{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.QueryClassic
  ( cmdQueryClassic
  ) where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Data.Semigroup            ((<>))
import HaskellWorks.Data.Sv.Char
import Options.Applicative       hiding (columns)

import qualified Data.ByteString.Char8              as C8
import qualified HaskellWorks.Data.Sv.Strict.Cursor as SVS
import qualified HaskellWorks.Data.Sv.Strict.Load   as SVS

repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f a = a:case f a of
  Just b  -> repeatedly f b
  Nothing -> []

runQueryClassic :: Bool -> [Int] -> FilePath -> Char -> IO ()
runQueryClassic createIndex columns filePath delimiter = do
  !cursor <- SVS.mmapDataFile delimiter createIndex filePath

  forM_ (repeatedly SVS.nextRow cursor) $ \row -> do
    let !fields = repeatedly SVS.nextField row
    let columnToFieldString column = maybe "" (C8.unpack . SVS.snippet) (drop column fields & listToMaybe)
    let !fieldStrings = columnToFieldString <$> columns
    putStrLn $ mconcat $ intersperse "|" fieldStrings

  return ()

cmdQueryClassic :: Mod CommandFields (IO ())
cmdQueryClassic = command "query-classic"  $ flip info idm $ runQueryClassic
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
