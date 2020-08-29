{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.QueryLazy
  ( cmdQueryLazy
  ) where

import App.Char
import App.Commands.Options.Parse
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource
import Data.Generics.Product.Any
import Options.Applicative          hiding (columns)

import qualified App.Commands.Options.Type                as Z
import qualified App.Data.ColumnDesc                      as Z
import qualified App.IO                                   as IO
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Builder                  as B
import qualified Data.ByteString.Lazy                     as LBS
import qualified Data.List                                as L
import qualified Data.Vector                              as DV
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor        as SVL
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor.Lazy   as SVLL
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor.Strict as SVLS
import qualified System.Exit                              as IO
import qualified System.IO                                as IO

{- HLINT ignore "Reduce duplication" -}

defaultMethod :: String
defaultMethod = "lazy-traverse"

runQueryLazy :: Z.QueryLazyOptions -> IO ()
runQueryLazy opts = case opts ^. the @"method" of
  "lazy-traverse"     -> runQueryLazyFast opts
  "strict-traverse"   -> runQueryLazySlow opts
  "strict-bytestring" -> runQueryLazyStrict opts
  method -> do
    IO.hPutStrLn IO.stderr $ "Unknown method: " <> method
    IO.exitFailure

runQueryLazySlow :: Z.QueryLazyOptions -> IO ()
runQueryLazySlow opts = do
  !bs <- IO.readInputFile (opts ^. the @"filePath")

  let !c = SVL.makeCursor (opts ^. the @"delimiter") bs
  let !rows = SVLL.toListVector c
  let !outDelimiterBuilder = B.word8 (opts ^. the @"outDelimiter")

  runResourceT $ do
    (_, hOut) <- IO.openOutputFile (opts ^. the @"outputFilePath") Nothing
    forM_ rows $ \row -> do
      let fieldStrings = columnToFieldString row <$> (opts ^.. the @"columns" . each . the @"number")

      liftIO $ B.hPutBuilder hOut $ mconcat (L.intersperse outDelimiterBuilder fieldStrings) <> B.word8 10

      return ()
  return ()
  where columnToFieldString :: DV.Vector LBS.ByteString -> Int -> B.Builder
        columnToFieldString fields i = if i >= 0 && i < DV.length fields
          then B.lazyByteString (DV.unsafeIndex fields i)
          else B.lazyByteString LBS.empty

runQueryLazyStrict :: Z.QueryLazyOptions -> IO ()
runQueryLazyStrict opts = do
  !bs <- IO.readInputFile (opts ^. the @"filePath")

  let !c = SVL.makeCursor (opts ^. the @"delimiter") bs
  let !rows = SVLS.toListVector c
  let !outDelimiterBuilder = B.word8 (opts ^. the @"outDelimiter")

  runResourceT $ do
    (_, hOut) <- IO.openOutputFile (opts ^. the @"outputFilePath") Nothing
    forM_ rows $ \row -> do
      let fieldStrings = columnToFieldString row <$> (opts ^.. the @"columns" . each . the @"number")

      liftIO $ B.hPutBuilder hOut $ mconcat (L.intersperse outDelimiterBuilder fieldStrings) <> B.word8 10

      return ()
  return ()
  where columnToFieldString :: DV.Vector BS.ByteString -> Int -> B.Builder
        columnToFieldString fields i = if i >= 0 && i < DV.length fields
          then B.byteString (DV.unsafeIndex fields i)
          else B.byteString BS.empty

runQueryLazyFast :: Z.QueryLazyOptions -> IO ()
runQueryLazyFast opts = do
  !bs <- IO.readInputFile (opts ^. the @"filePath")

  let !c = SVL.makeCursor (opts ^. the @"delimiter") bs
  let !sel = opts ^. the @"columns" <&> Z.realiseColumnDescLazy <&> Z.columnDescToTuple
  let !rows = SVLL.mapSelectListList sel c
  let !outDelimiterBuilder = B.word8 (opts ^. the @"outDelimiter")

  runResourceT $ do
    (_, hOut) <- IO.openOutputFile (opts ^. the @"outputFilePath") Nothing
    forM_ rows $ \row -> do
      let fieldStrings = fmap B.lazyByteString row

      liftIO $ B.hPutBuilder hOut $ mconcat (L.intersperse outDelimiterBuilder fieldStrings) <> B.word8 10

      return ()
  return ()

cmdQueryLazy :: Mod CommandFields (IO ())
cmdQueryLazy = command "query-lazy" $ flip info idm $ runQueryLazy <$> optsQueryLazy

optsQueryLazy :: Parser Z.QueryLazyOptions
optsQueryLazy = Z.QueryLazyOptions
    <$> many
        ( columnDesc
          (   long "column"
          <>  short 'k'
          <>  help "Column to select"
          <>  metavar "COLUMN INDEX" ))
    <*> strOption
          (   long "input"
          <>  short 'i'
          <>  help "Input DSV file"
          <>  metavar "FILE"
          <>  showDefault
          <>  value "-"
          )
    <*> strOption
          (   long "output"
          <>  short 'o'
          <>  help "Output DSV file"
          <>  metavar "FILE"
          <>  showDefault
          <>  value "-"
          )
    <*> option readWord8
          (   long "input-delimiter"
          <>  short 'd'
          <>  help "Input DSV delimiter"
          <>  metavar "CHAR"
          )
    <*> option readWord8
          (   long "output-delimiter"
          <>  short 'e'
          <>  help "Output DSV delimiter"
          <>  metavar "CHAR"
          )
    <*> strOption
          (   long "method"
          <>  short 'm'
          <>  help "Method"
          <>  metavar "METHOD"
          <>  showDefault
          <>  value defaultMethod
          )
