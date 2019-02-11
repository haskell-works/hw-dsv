{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import App.Char
import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.Semigroup                    ((<>))
import HaskellWorks.Data.ByteString.Lazy
import Options.Applicative               hiding (columns)

import qualified App.Commands.Options.Type         as Z
import qualified App.IO                            as IO
import qualified Data.ByteString.Builder           as B
import qualified Data.ByteString.Lazy              as LBS
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor as SVL
import qualified System.IO                         as IO

runCreateIndex :: Z.CreateIndexOptions -> IO ()
runCreateIndex opts = do
  let !filePath   = opts ^. the @"filePath"
  let !delimiter  = opts ^. the @"delimiter"

  !bs <- IO.readInputFile filePath

  let !cursor   = SVL.makeCursor delimiter bs
  let !markers  = cursor & SVL.dsvCursorMarkers
  let !newlines = cursor & SVL.dsvCursorNewlines
  let !filePathNoDash = if filePath == "-" then "stdin" else filePath

  hOutMarkers  <- IO.openFile (filePathNoDash ++ ".markers.idx") IO.WriteMode
  hOutNewlines <- IO.openFile (filePathNoDash ++ ".newlines.idx") IO.WriteMode

  forM_ (zip markers newlines) $ \(markerV, newlineV) -> do
    LBS.hPut hOutMarkers  (toLazyByteString markerV)
    LBS.hPut hOutNewlines (toLazyByteString newlineV)

  B.hPutBuilder hOutMarkers (B.word8 0xff) -- Telomere byte
  IO.hClose hOutMarkers

  B.hPutBuilder hOutNewlines (B.word8 0xff) -- Telomere byte
  IO.hClose hOutNewlines

  return ()

optsCreateIndex :: Parser Z.CreateIndexOptions
optsCreateIndex = Z.CreateIndexOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input DSV file"
        <>  metavar "STRING"
        )
  <*> option readWord8
        (   long "input-delimiter"
        <>  short 'd'
        <>  help "DSV delimiter"
        <>  metavar "CHAR"
        )

cmdCreateIndex :: Mod CommandFields (IO ())
cmdCreateIndex = command "create-index"  $ flip info idm $ runCreateIndex <$> optsCreateIndex
