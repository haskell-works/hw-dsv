{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.QueryLazy9
  ( cmdQueryLazy9
  ) where

import App.Commands.Options.Type
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource
import Data.Char
import Data.List
import Data.Semigroup               ((<>))
import HaskellWorks.Data.Sv.Char
import Options.Applicative          hiding (columns)

import qualified App.Commands.Options.Lens          as L
import qualified App.IO                             as IO
import qualified Data.ByteString.Builder            as B
import qualified Data.ByteString.Lazy               as LBS
import qualified Data.Vector                        as DV
import qualified HaskellWorks.Data.Sv.Lazy.Internal as LI

runQueryLazy9 :: QueryLazy9Options -> IO ()
runQueryLazy9 opts = do
  !bs <- LBS.readFile (opts ^. L.filePath)

  let !c = LI.makeLazyCursor (opts ^. L.delimiter) bs
  let !rows = LI.toListVector c
  let !outDelimiterBuilder = B.word8 (fromIntegral (ord (opts ^. L.outDelimiter)))

  runResourceT $ do
    (_, hOut) <- IO.openOutputFile (opts ^. L.outputFilePath) Nothing
    forM_ rows $ \row -> do
      let fieldStrings = columnToFieldString row <$> (opts ^. L.columns)

      liftIO $ B.hPutBuilder hOut $ mconcat (intersperse outDelimiterBuilder fieldStrings) <> B.word8 10

      return ()
  return ()

  where columnToFieldString :: DV.Vector LBS.ByteString -> Int -> B.Builder
        columnToFieldString fields i = if i >= 0 && i < DV.length fields
          then B.lazyByteString (DV.unsafeIndex fields i)
          else B.lazyByteString (LBS.empty)

cmdQueryLazy9 :: Mod CommandFields (IO ())
cmdQueryLazy9 = command "query-lazy-9" $ flip info idm $ runQueryLazy9 <$> optsQueryLazy9

optsQueryLazy9 :: Parser QueryLazy9Options
optsQueryLazy9 = QueryLazy9Options
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
    <*> option readChar
          (   long "out-delimiter"
          <>  help "DSV delimiter"
          <>  metavar "CHAR"
          )
