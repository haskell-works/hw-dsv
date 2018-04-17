{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Sv.Load
  ( SvCursor(..)
  , SvMode(..)
  , toInterestBitsVector
  , toInterestBits
  , loadFileWithNewIndex
  , mmapDataFile
  , mkInterestBits
  , boolsToVector
  ) where

import Data.Word
import Foreign.ForeignPtr
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Char
import HaskellWorks.Data.Sv.Cursor.Type
import System.IO.MMap

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BSI
import qualified Data.Vector.Storable     as DVS

{-# ANN module ("HLint: ignore Redundant guard"        :: String) #-}

boolsToVector :: Int -> [Bool] -> DVS.Vector Word64
boolsToVector n = DVS.unfoldrN vLen (go 0 0)
  where vLen = (n `div` 64) + 1
        go :: Word64 -> Word64 -> [Bool] -> Maybe (Word64, [Bool])
        go 64 w cs         = Just (w, cs)
        go _ w []          = Just (w, [])
        go n' w (True :cs) = go (n' + 1) ((1 .<. n') .|. w) cs
        go n' w (False:cs) = go (n' + 1)                 w  cs

toInterestBitsVector :: Word8 -> BS.ByteString -> DVS.Vector Word64
toInterestBitsVector delimiter bs = boolsToVector (BS.length bs) (toInterestBits delimiter SvUnquoted bs)

toInterestBits :: Word8 -> SvMode -> BS.ByteString -> [Bool]
toInterestBits delimiter mode text = case BS.uncons text of
  Just (a, as) -> case mode of
    SvUnquoted | a == dQuote    -> False:toInterestBits delimiter SvQuoted   as
    SvUnquoted | a == delimiter -> True :toInterestBits delimiter SvUnquoted as
    SvUnquoted | a == newline   -> True :toInterestBits delimiter SvUnquoted as
    SvUnquoted | True           -> False:toInterestBits delimiter SvUnquoted as
    SvQuoted   | a == dQuote    -> False:toInterestBits delimiter SvUnquoted as
    SvQuoted   | True           -> False:toInterestBits delimiter SvQuoted   as
  _            -> []

loadFileWithNewIndex :: Word8 -> FilePath -> IO (SvCursor BS.ByteString (DVS.Vector Word64))
loadFileWithNewIndex delimiter filePath = do
  text <- BS.readFile filePath
  return SvCursor
    { svCursorDelimiter     = delimiter
    , svCursorText          = text
    , svCursorInterestBits  = toInterestBitsVector delimiter text
    , svCursorPosition      = 1
    }

loadIbIndex :: FilePath -> IO (DVS.Vector Word64)
loadIbIndex filePath = fromForeignRegion <$> mmapFileForeignPtr (filePath ++ ".ib") ReadOnly Nothing

mkInterestBits :: Word8 -> Bool -> FilePath -> IO (DVS.Vector Word64)
mkInterestBits delimiter createIndex filePath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filePath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  !ibIndex <- if createIndex
    then return $ toInterestBitsVector delimiter bs
    else loadIbIndex (filePath ++ ".ib")
  return ibIndex

mmapDataFile :: Word8 -> Bool -> FilePath -> IO (SvCursor BS.ByteString CsPoppy)
mmapDataFile delimiter createIndex filePath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filePath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  !ibIndex <- makeCsPoppy <$> if createIndex
    then return $ toInterestBitsVector delimiter bs
    else loadIbIndex (filePath ++ ".ib")
  return SvCursor
    { svCursorDelimiter     = delimiter
    , svCursorText          = bs
    , svCursorInterestBits  = ibIndex
    , svCursorPosition      = 0
    }
