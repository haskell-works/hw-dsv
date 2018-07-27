{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import App.Char
import App.Commands.Options.Type (CreateIndexOptions (CreateIndexOptions))
import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.Semigroup            ((<>))
import Options.Applicative       hiding (columns)

import qualified App.IO                            as IO
import qualified Data.ByteString.Builder           as B
import qualified Data.Vector.Storable              as DVS
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor as SVL
import qualified System.IO                         as IO

runCreateIndex :: CreateIndexOptions -> IO ()
runCreateIndex opts = do
  let !filePath   = opts ^. the @"filePath"
  let !delimiter  = opts ^. the @"delimiter"

  !bs <- IO.readInputFile filePath

  let !cursor   = SVL.makeCursor delimiter bs
  let !markers  = cursor & SVL.dsvCursorMarkers
  let !newlines = cursor & SVL.dsvCursorNewlines

  hOutMarkers <- IO.openFile (filePath ++ ".markers.idx") IO.WriteMode
  forM_ (markers >>= DVS.toList) $ \w ->
    B.hPutBuilder hOutMarkers (B.word64LE w)
  B.hPutBuilder hOutMarkers (B.word8 0xff) -- Telomere byte
  IO.hClose hOutMarkers

  hOutNewlines <- IO.openFile (filePath ++ ".newlines.idx") IO.WriteMode
  forM_ (newlines >>= DVS.toList) $ \w ->
    B.hPutBuilder hOutNewlines (B.word64LE w)
  B.hPutBuilder hOutNewlines (B.word8 0xff) -- Telomere byte
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
