{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Data.Sv.Internal where

import Data.Bits                        (popCount)
import Data.Char                        (ord)
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.Sv.Bits
import HaskellWorks.Data.Sv.Broadword
import HaskellWorks.Data.Sv.Cursor.Type

import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Unsafe              as BSU
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.FromForeignRegion as IO
import qualified HaskellWorks.Data.Length            as V
import qualified HaskellWorks.Data.Sv.Char           as C
import qualified HaskellWorks.Data.Sv.Char.Word64    as CW

toInterestBitsVector :: Word8 -> BS.ByteString -> DVS.Vector Word64
toInterestBitsVector delimiter bs = DVS.fromListN vLen (toInterestBits64 delimiter bs)
  where vLen = (BS.length bs `div` 64) + 1

toInterestBits :: Word8 -> SvMode -> BS.ByteString -> [Bool]
toInterestBits delimiter mode text = case BS.uncons text of
  Just (a, as) -> case mode of
    SvUnquoted | a == C.doubleQuote -> False:toInterestBits delimiter SvQuoted   as
    SvUnquoted | a == delimiter     -> True :toInterestBits delimiter SvUnquoted as
    SvUnquoted | a == C.newline     -> True :toInterestBits delimiter SvUnquoted as
    SvUnquoted | True               -> False:toInterestBits delimiter SvUnquoted as
    SvQuoted   | a == C.doubleQuote -> False:toInterestBits delimiter SvUnquoted as
    SvQuoted   | True               -> False:toInterestBits delimiter SvQuoted   as
  _            -> []

toInterestBits64 :: Word8 -> BS.ByteString -> [Word64]
toInterestBits64 delimiter = go 0 0 SvUnquoted
  where go :: Int -> Word64 -> SvMode -> BS.ByteString -> [Word64]
        go n w mode text = case BS.uncons text of
          Just (a, as) -> case mode of
            SvUnquoted | a == C.doubleQuote -> cont n w 0 SvQuoted   as
            SvUnquoted | a == delimiter     -> cont n w 1 SvUnquoted as
            SvUnquoted | a == C.newline     -> cont n w 1 SvUnquoted as
            SvUnquoted | otherwise          -> cont n w 0 SvUnquoted as
            SvQuoted   | a == C.doubleQuote -> cont n w 0 SvUnquoted as
            SvQuoted   | otherwise          -> cont n w 0 SvQuoted   as
          Nothing      -> [w]
        cont :: Int -> Word64 -> Word64 -> SvMode -> BS.ByteString -> [Word64]
        cont n w b m bs = let nw = (b .<. fromIntegral n) .|. w in if n < 63
          then    go (n + 1) nw m bs
          else nw:go      0  0  m bs

boolsToVector :: Int -> [Bool] -> DVS.Vector Word64
boolsToVector n = DVS.unfoldrN vLen (go 0 0)
  where vLen = (n `div` 64) + 1
        go :: Word64 -> Word64 -> [Bool] -> Maybe (Word64, [Bool])
        go 64 w cs         = Just (w, cs)
        go _ w []          = Just (w, [])
        go n' w (True :cs) = go (n' + 1) ((1 .<. n') .|. w) cs
        go n' w (False:cs) = go (n' + 1)                 w  cs

mkInterestBits :: Word8 -> Bool -> FilePath -> IO (DVS.Vector Word64)
mkInterestBits delimiter createIndex filePath = do
  !bs <- IO.mmapFromForeignRegion filePath
  !ibIndex <- if createIndex
    then return $ toInterestBitsVector delimiter bs
    else IO.mmapFromForeignRegion (filePath ++ ".ib")
  return ibIndex

fillWord64WithChar8 :: Char -> Word64
fillWord64WithChar8 c = fillWord64 (fromIntegral (ord c) :: Word8)

mkDsvInterestBits :: Char -> DVS.Vector Word64 -> DVS.Vector Word64
mkDsvInterestBits delimiter v = DVS.fromListN ((DVS.length v + 7) `div` 8) $ mkDsvInterestBitsByWord64s
  CW.doubleQuote
  CW.newline
  (fillWord64WithChar8 delimiter)
  0
  0
  v

-- rdqs: repeated double quotes
-- rnls: repeated new lines
-- rdls: repeated delimiters
-- numQuotes: Number of quotes since beginning
-- n: Number of rank select bit string words since beginning
mkDsvInterestBitsByWord64s :: Word64 -> Word64 -> Word64 -> Count -> Position -> DVS.Vector Word64 -> [Word64]
mkDsvInterestBitsByWord64s rdqs rnls rdls numQuotes n ws | n < V.end ws =
  let w0    = atIndexOr 0 ws n
      w0Dqs = testWord8s (w0 .^. rdqs)
      w0Nls = testWord8s (w0 .^. rnls)
      w0Dls = testWord8s (w0 .^. rdls)
      w1    = atIndexOr 0 ws (n + 1)
      w1Dqs = testWord8s (w1 .^. rdqs)
      w1Nls = testWord8s (w1 .^. rnls)
      w1Dls = testWord8s (w1 .^. rdls)
      w2    = atIndexOr 0 ws (n + 2)
      w2Dqs = testWord8s (w2 .^. rdqs)
      w2Nls = testWord8s (w2 .^. rnls)
      w2Dls = testWord8s (w2 .^. rdls)
      w3    = atIndexOr 0 ws (n + 3)
      w3Dqs = testWord8s (w3 .^. rdqs)
      w3Nls = testWord8s (w3 .^. rnls)
      w3Dls = testWord8s (w3 .^. rdls)
      w4    = atIndexOr 0 ws (n + 4)
      w4Dqs = testWord8s (w4 .^. rdqs)
      w4Nls = testWord8s (w4 .^. rnls)
      w4Dls = testWord8s (w4 .^. rdls)
      w5    = atIndexOr 0 ws (n + 5)
      w5Dqs = testWord8s (w5 .^. rdqs)
      w5Nls = testWord8s (w5 .^. rnls)
      w5Dls = testWord8s (w5 .^. rdls)
      w6    = atIndexOr 0 ws (n + 6)
      w6Dqs = testWord8s (w6 .^. rdqs)
      w6Nls = testWord8s (w6 .^. rnls)
      w6Dls = testWord8s (w6 .^. rdls)
      w7    = atIndexOr 0 ws (n + 7)
      w7Dqs = testWord8s (w7 .^. rdqs)
      w7Nls = testWord8s (w7 .^. rnls)
      w7Dls = testWord8s (w7 .^. rdls)
      wDqs  = (w7Dqs  .<. 56) .|. (w6Dqs .<. 48) .|. (w5Dqs .<. 40) .|. (w4Dqs .<. 32) .|. (w3Dqs .<. 24) .|. (w2Dqs .<. 16) .|. (w1Dqs .<. 8) .|. w0Dqs
      wNls  = (w7Nls  .<. 56) .|. (w6Nls .<. 48) .|. (w5Nls .<. 40) .|. (w4Nls .<. 32) .|. (w3Nls .<. 24) .|. (w2Nls .<. 16) .|. (w1Nls .<. 8) .|. w0Nls
      wDls  = (w7Dls  .<. 56) .|. (w6Dls .<. 48) .|. (w5Dls .<. 40) .|. (w4Dls .<. 32) .|. (w3Dls .<. 24) .|. (w2Dls .<. 16) .|. (w1Dls .<. 8) .|. w0Dls
      numWordQuotes = comp wDqs
      wMask = toggle64 numQuotes numWordQuotes
      newNumQuotes = numQuotes + fromIntegral (popCount numWordQuotes)
  in  (comp (wNls .&. wDls) .&. wMask):mkDsvInterestBitsByWord64s rdqs rnls rdls newNumQuotes (n + 8) ws
mkDsvInterestBitsByWord64s _ _ _ _ _ _ = []

word8To64 :: Word8 -> Word64
word8To64 = fromIntegral

lazyByteStringToWord64s1 :: LBS.ByteString -> [Word64]
lazyByteStringToWord64s1 lbs = case LBS.splitAt 8 lbs of
  (lcs, lds)
    | LBS.length lcs == 8 ->
        let w = (word8To64 (lcs `LBS.index` 0) .<.  0) .|.
                (word8To64 (lcs `LBS.index` 1) .<.  8) .|.
                (word8To64 (lcs `LBS.index` 2) .<. 16) .|.
                (word8To64 (lcs `LBS.index` 3) .<. 24) .|.
                (word8To64 (lcs `LBS.index` 4) .<. 32) .|.
                (word8To64 (lcs `LBS.index` 5) .<. 40) .|.
                (word8To64 (lcs `LBS.index` 6) .<. 48) .|.
                (word8To64 (lcs `LBS.index` 7) .<. 56)
        in w:lazyByteStringToWord64s1 lds
    | otherwise -> [LBS.foldl' go 0 lcs]
  where go :: Word64 -> Word8 -> Word64
        go w b = (w .<. 8) .|. word8To64 b

lazyByteStringToWord64s2 :: LBS.ByteString -> [Word64]
lazyByteStringToWord64s2 lbs = case LBS.splitAt 8 lbs of
  (lcs, lds)
    | LBS.length lcs == 8 -> case LBS.toStrict lcs of
        cs ->
          let w = (word8To64 (cs `BSU.unsafeIndex` 0) .<.  0) .|.
                  (word8To64 (cs `BSU.unsafeIndex` 1) .<.  8) .|.
                  (word8To64 (cs `BSU.unsafeIndex` 2) .<. 16) .|.
                  (word8To64 (cs `BSU.unsafeIndex` 3) .<. 24) .|.
                  (word8To64 (cs `BSU.unsafeIndex` 4) .<. 32) .|.
                  (word8To64 (cs `BSU.unsafeIndex` 5) .<. 40) .|.
                  (word8To64 (cs `BSU.unsafeIndex` 6) .<. 48) .|.
                  (word8To64 (cs `BSU.unsafeIndex` 7) .<. 56)
          in w:lazyByteStringToWord64s2 lds
    | otherwise -> [LBS.foldl' go 0 lcs]
  where go :: Word64 -> Word8 -> Word64
        go w b = (w .<. 8) .|. word8To64 b

lazyByteStringToWord64s3 :: LBS.ByteString -> [Word64]
lazyByteStringToWord64s3 lbs = case LBS.splitAt 8 lbs of
  (lcs, lds) -> case LBS.toStrict lcs of
    cs -> BS.foldl' go 0 cs:if LBS.null lds then [] else lazyByteStringToWord64s3 lds
  where go :: Word64 -> Word8 -> Word64
        go w b = (w .<. 8) .|. word8To64 b
