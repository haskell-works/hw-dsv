{-# LANGUAGE BangPatterns #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import App.Char
import App.Commands.Options.Type
import Control.Lens
import Control.Monad
import Data.Semigroup            ((<>))
import Options.Applicative       hiding (columns)

import qualified App.IO                                 as IO
import qualified App.Lens                               as L
import qualified Data.ByteString.Builder                as B
import qualified Data.Vector.Storable                   as DVS
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor      as SVL
import qualified System.IO                              as IO

runCreateIndex :: CreateIndexOptions -> IO ()
runCreateIndex opts = do
  let !filePath   = opts ^. L.filePath
  let !delimiter  = opts ^. L.delimiter

  !bs <- IO.readInputFile filePath

  let !cursor   = SVL.makeCursor delimiter bs
  let !markers  = cursor & SVL.dsvCursorMarkers
  let !newlines = cursor & SVL.dsvCursorNewlines

  hOutMarkers <- IO.openFile (filePath ++ ".markers.idx") IO.WriteMode
  forM_ (markers >>= DVS.toList) $ \w -> do
    B.hPutBuilder hOutMarkers (B.word64LE w)
  IO.hClose hOutMarkers

  hOutNewlines <- IO.openFile (filePath ++ ".newlines.idx") IO.WriteMode
  forM_ (newlines >>= DVS.toList) $ \w -> do
    B.hPutBuilder hOutNewlines (B.word64LE w)
  IO.hClose hOutNewlines

  return ()

optsCreateIndex :: Parser CreateIndexOptions
optsCreateIndex = CreateIndexOptions
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
