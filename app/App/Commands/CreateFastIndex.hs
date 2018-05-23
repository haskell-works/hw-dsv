{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.CreateFastIndex
  ( cmdCreateFastIndex
  ) where

import App.Commands.Options.Type
import Control.Lens
import Data.Semigroup                     ((<>))
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Sv.Internal.Char
import Options.Applicative                hiding (columns)

import qualified App.Commands.Options.Lens                   as L
import qualified Data.ByteString.Builder                     as B
import qualified Data.Vector.Storable                        as DVS
import qualified HaskellWorks.Data.FromForeignRegion         as IO
import qualified HaskellWorks.Data.Sv.Internal.Char.Word64   as C
import qualified HaskellWorks.Data.Sv.Strict.Cursor.Internal as SVS
import qualified System.IO                                   as IO

writeBuilder :: FilePath -> B.Builder -> IO ()
writeBuilder fp b = do
  h <- IO.openFile fp IO.WriteMode
  B.hPutBuilder h b
  IO.hClose h

runCreateFastIndex :: CreateFastIndexOptions -> IO ()
runCreateFastIndex opts = do
  let filePath  = opts ^. L.filePath
  let delimiter = opts ^. L.delimiter & SVS.fillWord64WithChar8

  !(v :: DVS.Vector Word64) <- IO.mmapFromForeignRegion filePath

  let !rawBits    = SVS.mkDsvRawBitsByWord64s C.doubleQuote C.newline delimiter v
  let !rawBitsLo  = DVS.map (\c -> fromIntegral ( c         .&. 0xffffffff)  :: Word32) rawBits
  let !rawBitsHi  = DVS.map (\c -> fromIntegral ((c .>. 32) .&. 0xffffffff)  :: Word32) rawBits
  let !cpcs       = SVS.mkCummulativeDqPopCount rawBits
  let !ib         = SVS.mkIbVector' rawBits cpcs v

  writeBuilder (filePath <> ".rb.idx")    $ foldMap B.word64LE (DVS.toList rawBits)
  writeBuilder (filePath <> ".rb.lo.idx") $ foldMap B.word32LE (DVS.toList rawBitsLo)
  writeBuilder (filePath <> ".rb.hi.idx") $ foldMap B.word32LE (DVS.toList rawBitsHi)
  writeBuilder (filePath <> ".pc.idx"   ) $ foldMap B.word64LE (DVS.toList cpcs)
  writeBuilder (filePath <> ".ib.idx"   ) $ foldMap B.word64LE (DVS.toList ib)

  return ()

optsCreateFastIndex :: Parser CreateFastIndexOptions
optsCreateFastIndex = CreateFastIndexOptions
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

cmdCreateFastIndex :: Mod CommandFields (IO ())
cmdCreateFastIndex = command "create-fast-index"  $ flip info idm $ runCreateFastIndex <$> optsCreateFastIndex
