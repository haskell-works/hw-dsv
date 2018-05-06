{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.QueryLazy
  ( cmdQueryLazy
  ) where

import App.Commands.Options.Type
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource
import Data.List
import Data.Maybe
import Data.Semigroup               ((<>))
import HaskellWorks.Data.Sv.Char
import HaskellWorks.Data.Sv.Cursor
import HaskellWorks.Data.Sv.Load
import Options.Applicative          hiding (columns)

import qualified App.Commands.Options.Lens as L
import qualified App.IO                    as IO
import qualified Data.ByteString.Builder   as B
import qualified Data.ByteString.Lazy      as LBS

toLazyCells :: LazyCursor -> [(LazyCursorType, LazyCursor)]
toLazyCells c = go (Just (LazyField, c))
  where go :: Maybe (LazyCursorType, LazyCursor) -> [(LazyCursorType, LazyCursor)]
        go mCell = case mCell of
          Just cell -> cell:go (lazyNext (snd cell))
          Nothing   -> []

isField :: (LazyCursorType, LazyCursor) -> Bool
isField (LazyField, _) = True
isField _              = False

toRowsAndFields :: [(LazyCursorType, LazyCursor)] -> [[LazyCursor]]
toRowsAndFields (x:xs) = case span isField xs of
  (fs, rs) -> (snd x:(snd <$> fs)):toRowsAndFields rs
toRowsAndFields [] = []

runQueryLazy :: QueryLazyOptions -> IO ()
runQueryLazy opts = do
  cursor <- loadLazyCursor (opts ^. L.delimiter) (opts ^. L.filePath)

  let rows = (lazySnippet <$>) <$> toRowsAndFields (toLazyCells cursor)

  runResourceT $ do
    (_, hOut) <- IO.openOutputFile (opts ^. L.outputFilePath) (opts ^. L.outputBufferSize)

    forM_ rows $ \row -> do
      let fieldStrings = columnToFieldString row <$> (opts ^. L.columns)

      liftIO $ B.hPutBuilder hOut $ mconcat (intersperse "|" fieldStrings) <> B.word8 10

      return ()

    return ()
  where columnToFieldString :: [LBS.ByteString] -> Int -> B.Builder
        columnToFieldString fields column = maybe mempty B.lazyByteString (drop column fields & listToMaybe)

cmdQueryLazy :: Mod CommandFields (IO ())
cmdQueryLazy = command "query-lazy" $ flip info idm $ runQueryLazy <$> optsQueryLazy

optsQueryLazy :: Parser QueryLazyOptions
optsQueryLazy = QueryLazyOptions
    <$> many
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
