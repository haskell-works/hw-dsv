{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.Data.Sv.Load
  ( SvCursor(..)
  , SvMode(..)
  , FillWord64(..)
  , loadFileWithNewIndex
  , mmapDataFile
  , mmapDataFile2
  , mkInterestBits
  , boolsToVector
  , mkDsvInterestBits
  ) where

import Data.Char                                 (ord)
import Data.Word
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Product
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Broadword
import HaskellWorks.Data.Sv.Cursor.Type
import HaskellWorks.Data.Sv.Internal

import qualified Data.ByteString                     as BS
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.FromForeignRegion as IO

{-# ANN module ("HLint: ignore Redundant guard"        :: String) #-}

loadFileWithNewIndex :: Word8 -> FilePath -> IO (SvCursor BS.ByteString (DVS.Vector Word64))
loadFileWithNewIndex delimiter filePath = do
  text <- BS.readFile filePath
  let ibIndex = toInterestBitsVector delimiter text
  return SvCursor
    { svCursorDelimiter     = delimiter
    , svCursorText          = text
    , svCursorInterestBits  = ibIndex
    , svCursorPosition      = 1
    , svCursorPopCount      = popCount1 ibIndex
    }

mmapDataFile :: Word8 -> Bool -> FilePath -> IO (SvCursor BS.ByteString CsPoppy)
mmapDataFile delimiter createIndex filePath = do
  !bs <- IO.mmapFromForeignRegion filePath
  !ibIndex <- makeCsPoppy <$> if createIndex
    then return $ toInterestBitsVector delimiter bs
    else IO.mmapFromForeignRegion (filePath ++ ".ib")
  return SvCursor
    { svCursorDelimiter     = delimiter
    , svCursorText          = bs
    , svCursorInterestBits  = ibIndex
    , svCursorPosition      = 0
    , svCursorPopCount      = popCount1 ibIndex
    }

mmapDataFile2 :: Char -> Bool -> FilePath -> IO (SvCursor BS.ByteString CsPoppy)
mmapDataFile2 delimiter createIndex filePath = do
  (!bs) :*: (!v) <- IO.mmapFromForeignRegion filePath
  let !_ = v :: DVS.Vector Word64
  !ibIndex <- makeCsPoppy <$> if createIndex
    then return $ mkDsvInterestBits delimiter v
    else IO.mmapFromForeignRegion (filePath ++ ".ib")
  return SvCursor
    { svCursorDelimiter     = fromIntegral (ord delimiter)
    , svCursorText          = bs
    , svCursorInterestBits  = ibIndex
    , svCursorPosition      = 0
    , svCursorPopCount      = popCount1 ibIndex
    }
