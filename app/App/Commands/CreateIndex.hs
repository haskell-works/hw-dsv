{-# LANGUAGE BangPatterns #-}

module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import App.Commands.Options.Type
import Control.Lens
import Control.Monad
import Data.Char
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Char
import HaskellWorks.Data.Sv.Load
import Options.Applicative                  hiding (columns)

import qualified App.Commands.Options.Lens        as L
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Builder          as B
import qualified Data.Vector.Storable             as DVS
import qualified HaskellWorks.Data.Sv.Cursor.Lens as LC
import qualified System.IO                        as IO

runCreateIndex :: CreateIndexOptions -> IO ()
runCreateIndex opts = do
  let filePath  = opts ^. L.filePath
  let delimiter = opts ^. L.delimiter

  !cursor <- if opts ^. L.classic
    then mmapDataFile  (fromIntegral (ord delimiter)) True filePath
    else mmapDataFile2                    delimiter   True filePath

  let ib = cursor ^. LC.interestBits

  hOut <- IO.openFile (filePath ++ ".idx") IO.WriteMode

  forM_ (DVS.toList (csPoppyBits ib)) $ \w -> do
    B.hPutBuilder hOut (B.word64LE w)

  IO.hClose hOut

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
  <*> switch (long "classic")

cmdCreateIndex :: Mod CommandFields (IO ())
cmdCreateIndex = command "create-index"  $ flip info idm $ runCreateIndex <$> optsCreateIndex
