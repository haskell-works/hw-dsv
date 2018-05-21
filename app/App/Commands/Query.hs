{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.Query
  ( cmdQuery
  ) where

import App.Commands.Options.Type
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class               (liftIO)
import Control.Monad.Trans.Resource
import Data.List
import Data.Maybe
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Char
import HaskellWorks.Data.Sv.Strict.Cursor
import HaskellWorks.Data.Sv.Strict.Load
import Options.Applicative                  hiding (columns)

import qualified App.Commands.Options.Lens as L
import qualified App.IO                    as IO
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Builder   as B

repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f a = a:case f a of
  Just b  -> repeatedly f b
  Nothing -> []

runQuery :: QueryOptions -> IO ()
runQuery opts = do
  cursor <- if opts ^. L.fast
    then mmapCursor    (opts ^. L.delimiter) (opts ^. L.createIndex) (opts ^. L.filePath)
    else mmapDataFile2 (opts ^. L.delimiter) (opts ^. L.createIndex) (opts ^. L.filePath)

  runResourceT $ do
    (_, hOut) <- IO.openOutputFile (opts ^. L.outputFilePath) (opts ^. L.outputBufferSize)

    forM_ (repeatedly nextRow cursor) $ \row -> do
      let fieldCursors = repeatedly nextField row
      let fieldStrings = columnToFieldString fieldCursors <$> (opts ^. L.columns)

      liftIO $ B.hPutBuilder hOut $ mconcat (intersperse "|" fieldStrings) <> B.word8 10

      return ()

    return ()
  where columnToFieldString :: [SvCursor BS.ByteString CsPoppy] -> Int -> B.Builder
        columnToFieldString fields column = maybe mempty (B.byteString . snippet) (drop column fields & listToMaybe)

cmdQuery :: Mod CommandFields (IO ())
cmdQuery = command "query" $ flip info idm $ runQuery <$> optsQuery

optsQuery :: Parser QueryOptions
optsQuery = QueryOptions
    <$> switch (long "create-index")
    <*> many
        ( option auto
          (   long "column"
          <>  short 'k'
          <>  help "Column to select"
          <>  metavar "COLUMN INDEX" ))
    <*> strOption
          (   long "source"
          <>  help "Separated Value file"
          <>  metavar "STRING"
          )
    <*> strOption
          (   long "target"
          <>  help "Separated Value file"
          <>  metavar "STRING"
          )
    <*> option readChar
          (   long "delimiter"
          <>  help "DSV delimiter"
          <>  metavar "CHAR"
          )
    <*> optional
          ( option auto
            (   long "output-buffer-size"
            <>  help "Output buffer size"
            <>  metavar "BYTES"
            )
          )
    <*> switch (long "fast")
