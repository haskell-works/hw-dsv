{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Sv
  ( SvCursor(..)
  , SvMode(..)
  , toInterestBitsVector
  , toInterestBits
  , loadFileWithNewIndex
  , mmapDataFile
  , next
  , snippet
  , mkInterestBits
  , boolsToVector
  , nextField
  , nextInterestingBit
  , wordAt
  , nextPosition
  , nextRow
  ) where

import Control.Lens
import Data.Word
import Foreign.ForeignPtr
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Char
import HaskellWorks.Data.Sv.Cursor.Type
import System.IO.MMap

import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Internal         as BSI
import qualified Data.Vector.Storable             as DVS
import qualified HaskellWorks.Data.AtIndex        as VL
import qualified HaskellWorks.Data.Sv.Cursor.Lens as L

{-# ANN module ("HLint: ignore Redundant guard"        :: String) #-}

boolsToVector :: Int -> [Bool] -> DVS.Vector Word64
boolsToVector n = DVS.unfoldrN vLen (go 0 0)
  where vLen = (n `div` 64) + 1
        go :: Word64 -> Word64 -> [Bool] -> Maybe (Word64, [Bool])
        go 64 w cs         = Just (w, cs)
        go _ w []          = Just (w, [])
        go n' w (True :cs) = go (n' + 1) ((1 .<. n') .|. w) cs
        go n' w (False:cs) = go (n' + 1)                 w  cs

toInterestBitsVector :: BS.ByteString -> DVS.Vector Word64
toInterestBitsVector bs = boolsToVector (BS.length bs) (toInterestBits SvUnquoted bs)

toInterestBits :: SvMode -> BS.ByteString -> [Bool]
toInterestBits mode text = case BS.uncons text of
  Just (a, as) -> case mode of
    SvUnquoted | a == dQuote  -> False:toInterestBits SvQuoted   as
    SvUnquoted | a == pipe    -> True :toInterestBits SvUnquoted as
    SvUnquoted | a == newline -> True :toInterestBits SvUnquoted as
    SvUnquoted | True         -> False:toInterestBits SvUnquoted as
    SvQuoted   | a == dQuote  -> False:toInterestBits SvUnquoted as
    SvQuoted   | True         -> False:toInterestBits SvQuoted   as
  _            -> []

loadFileWithNewIndex :: FilePath -> IO (SvCursor BS.ByteString (DVS.Vector Word64))
loadFileWithNewIndex filePath = do
  text <- BS.readFile filePath
  return SvCursor
    { svCursorText          = text
    , svCursorInterestBits  = toInterestBitsVector text
    , svCursorPosition      = 1
    }

loadIbIndex :: FilePath -> IO (DVS.Vector Word64)
loadIbIndex filePath = fromForeignRegion <$> mmapFileForeignPtr (filePath ++ ".ib") ReadOnly Nothing

mkInterestBits :: Bool -> FilePath -> IO (DVS.Vector Word64)
mkInterestBits createIndex filePath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filePath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  !ibIndex <- if createIndex
    then return $ toInterestBitsVector bs
    else loadIbIndex (filePath ++ ".ib")
  return ibIndex

mmapDataFile :: Bool -> FilePath -> IO (SvCursor BS.ByteString CsPoppy)
mmapDataFile createIndex filePath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filePath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  !ibIndex <- makeCsPoppy <$> if createIndex
    then return $ toInterestBitsVector bs
    else loadIbIndex (filePath ++ ".ib")
  return SvCursor
    { svCursorText          = bs
    , svCursorInterestBits  = ibIndex
    , svCursorPosition      = 0
    }

next :: (Rank1 s, Select1 s) => SvCursor t s -> SvCursor t s
next cursor = cursor
  { svCursorPosition = newPos
  }
  where currentRank = rank1   (svCursorInterestBits cursor) (svCursorPosition cursor)
        newPos      = select1 (svCursorInterestBits cursor) (currentRank + 1)

nextInterestingBit :: (Rank1 s, Select1 s) => SvCursor t s -> Maybe (SvCursor t s)
nextInterestingBit cursor = Just cursor
  { svCursorPosition = newPos
  }
  where currentRank = rank1   (svCursorInterestBits cursor) (svCursorPosition cursor)
        newPos      = select1 (svCursorInterestBits cursor) (currentRank + 1) - 1

nextPosition :: SvCursor BS.ByteString s -> Maybe (SvCursor BS.ByteString s)
nextPosition cursor = if newPos < fromIntegral (VL.length (svCursorText cursor))
  then Just cursor
    { svCursorPosition = newPos
    }
  else Nothing
  where newPos = (svCursorPosition cursor + 1) `min` fromIntegral (cursor ^. L.text & BS.length)

nextField :: (Rank1 s, Select1 s) => SvCursor BS.ByteString s -> Maybe (SvCursor BS.ByteString s)
nextField c = do
  ibCursor <- nextInterestingBit c
  ibWord <- wordAt ibCursor
  if ibWord == pipe
    then nextPosition ibCursor
    else Nothing

nextRow :: (Rank1 s, Select1 s) => SvCursor BS.ByteString s -> Maybe (SvCursor BS.ByteString s)
nextRow c = do
  ibCursor <- nextInterestingBit c
  ibWord <- wordAt ibCursor
  newCursor <- nextPosition ibCursor
  if ibWord == newline
    then Just newCursor
    else if atEnd newCursor
      then Nothing
      else nextRow newCursor

atEnd :: SvCursor BS.ByteString s -> Bool
atEnd c = c ^. L.position >= fromIntegral (BS.length (c ^. L.text))

wordAt :: SvCursor BS.ByteString s -> Maybe Word8
wordAt c = if pos < fromIntegral (BS.length txt)
  then Just (txt VL.!!! fromIntegral pos)
  else Nothing
  where pos = c ^. L.position
        txt = c ^. L.text

snippet :: SvCursor BS.ByteString CsPoppy -> BS.ByteString
snippet c = BS.take (len `max` 0) $ BS.drop posC $ svCursorText c
  where d = next c
        posC = fromIntegral $ svCursorPosition c
        posD = fromIntegral $ svCursorPosition d
        len  = posD - posC - 1
