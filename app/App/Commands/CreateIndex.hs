{-# LANGUAGE BangPatterns #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import App.Commands.Options.Type
import Control.Lens
import Control.Monad
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Internal.Char
import Options.Applicative                  hiding (columns)

import qualified App.Commands.Options.Internal           as I
import qualified App.Commands.Options.Lens               as L
import qualified App.IO                                  as IO
import qualified Data.ByteString.Builder                 as B
import qualified Data.Vector.Storable                    as DVS
import qualified HaskellWorks.Data.Sv.Lazy.Cursor        as SVL
import qualified HaskellWorks.Data.Sv.Lazy.Cursor.Lens   as SVLL
import qualified HaskellWorks.Data.Sv.Strict.Cursor.Lens as SVSL
import qualified HaskellWorks.Data.Sv.Strict.Load        as SVS
import qualified System.IO                               as IO

runCreateIndex :: CreateIndexOptions -> IO ()
runCreateIndex opts = do
  let filePath  = opts ^. L.filePath
  let delimiter = opts ^. L.delimiter

  case opts ^. L.indexMethod of
    I.Classic -> do
      !cursor <- SVS.mmapDataFile   delimiter True filePath

      let markers = cursor ^. SVSL.markers

      hOut <- IO.openFile (filePath ++ ".idx") IO.WriteMode

      forM_ (DVS.toList (csPoppyBits markers)) $ \w -> do
        B.hPutBuilder hOut (B.word64LE w)

      IO.hClose hOut

      return ()
    I.Strict  -> do
      !cursor <- SVS.mmapDataFile2  delimiter True filePath
      let markers = cursor ^. SVSL.markers

      hOut <- IO.openFile (filePath ++ ".idx") IO.WriteMode

      forM_ (DVS.toList (csPoppyBits markers)) $ \w -> do
        B.hPutBuilder hOut (B.word64LE w)

      IO.hClose hOut

      return ()
    I.Lazy    -> do
      !bs <- IO.readInputFile (opts ^. L.filePath)

      let !cursor = SVL.makeCursor (opts ^. L.delimiter) bs

      let markers   = cursor ^. SVLL.markers
      let newlines  = cursor ^. SVLL.newlines

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
        (   long "file"
        <>  help "Separated Value file"
        <>  metavar "STRING"
        )
  <*> option readChar
        (   long "delimiter"
        <>  help "DSV delimiter"
        <>  metavar "CHAR"
        )
  <*> option auto
        (   long "index-method"
        <>  help "Index method"
        <>  metavar "INDEX_METHOD"
        )

cmdCreateIndex :: Mod CommandFields (IO ())
cmdCreateIndex = command "create-index"  $ flip info idm $ runCreateIndex <$> optsCreateIndex
