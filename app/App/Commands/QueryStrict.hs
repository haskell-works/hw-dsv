{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module App.Commands.QueryStrict
  ( cmdQueryStrict
  ) where

import App.Char
import App.Commands.Options.Parse
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class       (liftIO)
import Control.Monad.Trans.Resource
import Data.Generics.Product.Any
import Data.List
import Options.Applicative

import qualified App.Commands.Options.Type           as Z
import qualified App.IO                              as IO
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Builder             as B
import qualified Data.Vector                         as DV
import qualified HaskellWorks.Data.Dsv.Strict.Cursor as SVS

runQueryStrict :: Z.QueryStrictOptions -> IO ()
runQueryStrict opts = do
  let delimiter     = opts ^. the @"delimiter"
  let inputFilePath = opts ^. the @"filePath"
  let useIndex      = opts ^. the @"useIndex"
  c <- SVS.mmapCursor delimiter useIndex inputFilePath

  let !rows = SVS.toListVector c
  let !outDelimiterBuilder = B.word8 (opts ^. the @"outDelimiter")

  runResourceT $ do
    (_, hOut) <- IO.openOutputFile (opts ^. the @"outputFilePath") Nothing
    forM_ rows $ \row -> do
      let fieldStrings = columnToFieldString row <$> (opts ^. the @"columns")

      liftIO $ B.hPutBuilder hOut $ mconcat (intersperse outDelimiterBuilder fieldStrings) <> B.word8 10

      return ()
  return ()

  where columnToFieldString :: DV.Vector BS.ByteString -> Int -> B.Builder
        columnToFieldString fields i = if i >= 0 && i < DV.length fields
          then B.byteString (DV.unsafeIndex fields i)
          else B.byteString  BS.empty

cmdQueryStrict :: Mod CommandFields (IO ())
cmdQueryStrict = command "query-strict" $ flip info idm $ runQueryStrict <$> optsQueryStrict

optsQueryStrict :: Parser Z.QueryStrictOptions
optsQueryStrict = Z.QueryStrictOptions
    <$> many
        ( nonZeroOneBased
          (   long "column"
          <>  short 'k'
          <>  help "Column to select"
          <>  metavar "COLUMN INDEX" ))
    <*> strOption
          (   long "input"
          <>  short 'i'
          <>  help "Input DSV file"
          <>  metavar "FILE"
          )
    <*> strOption
          (   long "output"
          <>  short 'o'
          <>  help "Output DSV file"
          <>  metavar "FILE"
          )
    <*> option readWord8
          (   long "input-delimiter"
          <>  short 'd'
          <>  help "DSV delimiter to read in the input"
          <>  metavar "CHAR"
          )
    <*> option readWord8
          (   long "output-delimiter"
          <>  short 'e'
          <>  help "DSV delimiter to write in the output"
          <>  metavar "CHAR"
          )
    <*> switch (long "use-index")
