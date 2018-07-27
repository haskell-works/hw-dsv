{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.QueryLazy
  ( cmdQueryLazy
  ) where

import App.Char
import App.Commands.Options.Type    (QueryLazyOptions (QueryLazyOptions))
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource
import Data.Generics.Product.Any
import Data.List
import Data.Semigroup               ((<>))
import Options.Applicative          hiding (columns)

import qualified App.IO                            as IO
import qualified Data.ByteString.Builder           as B
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.Vector                       as DV
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor as SVL

runQueryLazy :: QueryLazyOptions -> IO ()
runQueryLazy opts = do
  !bs <- IO.readInputFile (opts ^. the @"filePath")

  let !c = SVL.makeCursor (opts ^. the @"delimiter") bs
  let !rows = SVL.toListVector c
  let !outDelimiterBuilder = B.word8 (opts ^. the @"outDelimiter")

  runResourceT $ do
    (_, hOut) <- IO.openOutputFile (opts ^. the @"outputFilePath") Nothing
    forM_ rows $ \row -> do
      let fieldStrings = columnToFieldString row <$> (opts ^. the @"columns")

      liftIO $ B.hPutBuilder hOut $ mconcat (intersperse outDelimiterBuilder fieldStrings) <> B.word8 10

      return ()
  return ()

  where columnToFieldString :: DV.Vector LBS.ByteString -> Int -> B.Builder
        columnToFieldString fields i = if i >= 0 && i < DV.length fields
          then B.lazyByteString (DV.unsafeIndex fields i)
          else B.lazyByteString (LBS.empty)

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
