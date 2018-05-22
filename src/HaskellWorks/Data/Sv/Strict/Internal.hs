{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Data.Sv.Strict.Internal where

import Control.Monad.State
import Data.Bits                               (popCount)
import Data.Char                               (ord)
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.Sv.Bits
import HaskellWorks.Data.Sv.Broadword
import HaskellWorks.Data.Sv.Strict.Cursor.Type
import Prelude

import qualified Data.Attoparsec.ByteString          as AP
import qualified Data.Attoparsec.Lazy                as APL
import qualified Data.ByteString                     as BS
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.FromForeignRegion as IO
import qualified HaskellWorks.Data.Length            as V
import qualified HaskellWorks.Data.Sv.Char           as C
import qualified HaskellWorks.Data.Sv.Char.Word64    as CW

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

toInterestBitsVector :: Word8 -> BS.ByteString -> DVS.Vector Word64
toInterestBitsVector delimiter bs = DVS.fromListN vLen (toInterestBits64 delimiter bs)
  where vLen = (BS.length bs `div` 64) + 1

toInterestBits :: Word8 -> SvMode -> BS.ByteString -> [Bool]
toInterestBits delimiter mode text = case BS.uncons text of
  Just (a, as) -> case mode of
    SvUnquoted | a == C.doubleQuote -> False:toInterestBits delimiter SvQuoted   as
    SvUnquoted | a == delimiter     -> True :toInterestBits delimiter SvUnquoted as
    SvUnquoted | a == C.newline     -> True :toInterestBits delimiter SvUnquoted as
    SvUnquoted | otherwise          -> False:toInterestBits delimiter SvUnquoted as
    SvQuoted   | a == C.doubleQuote -> False:toInterestBits delimiter SvUnquoted as
    SvQuoted   | otherwise          -> False:toInterestBits delimiter SvQuoted   as
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

parserWord64 :: APL.Parser Word64
parserWord64 = do
  a <- AP.anyWord8
  b <- AP.anyWord8
  c <- AP.anyWord8
  d <- AP.anyWord8
  e <- AP.anyWord8
  f <- AP.anyWord8
  g <- AP.anyWord8
  h <- AP.anyWord8
  return $   fromIntegral  a           .|.
            (fromIntegral (b .<.  8))  .|.
            (fromIntegral (c .<. 16))  .|.
            (fromIntegral (d .<. 24))  .|.
            (fromIntegral (e .<. 32))  .|.
            (fromIntegral (f .<. 40))  .|.
            (fromIntegral (g .<. 48))  .|.
            (fromIntegral (h .<. 56))

realignByteStrings :: Int -> [BS.ByteString] -> [BS.ByteString]
realignByteStrings n = go
  where go (bs:xss)    | BS.length bs == 0 = go xss
        go (bs:xss)    | BS.length bs `mod` n == 0 = bs:go xss
        go (bs:xss)    | BS.length bs > n = case BS.splitAt n bs of (cs, ds) -> cs:go (ds:xss)
        go (bs:cs:css) | BS.length bs + BS.length cs <= n = go (BS.append bs cs:css)
        go (bs:cs:css) = case BS.splitAt (n - BS.length bs) cs of (ds, es) -> BS.append bs ds:go (es:css)
        go [bs]        | BS.length bs > 0 = [bs]
        go _           = []

unsafeIndex :: DVS.Vector Word64 -> Int -> Word64
unsafeIndex v i | i < 0                           = error $ "Invalid index: " <> show i <> " for vector sized " <> show (DVS.length v)
unsafeIndex v i | fromIntegral i >= DVS.length v  = error $ "Invalid index: " <> show i <> " for vector sized " <> show (DVS.length v)
unsafeIndex v i | otherwise                       = DVS.unsafeIndex v (fromIntegral i)
-- unsafeIndex v i = DVS.unsafeIndex v (fromIntegral i)
{-# INLINE unsafeIndex #-}

dvsLength :: DVS.Vector Word64 -> Int
dvsLength v = fromIntegral (DVS.length v)
{-# INLINE dvsLength #-}

atIndexOr2 :: Word64 -> DVS.Vector Word64 -> Int -> Word64
atIndexOr2 d _ i | i < 0                           = d
atIndexOr2 d v i | fromIntegral i >= DVS.length v  = d
atIndexOr2 _ v i | otherwise                       = unsafeIndex v (fromIntegral i)
{-# NOINLINE atIndexOr2 #-}

-- rdqs: repeated double quotes
-- rnls: repeated new lines
-- rdls: repeated delimiters
-- numQuotes: Number of quotes since beginning
-- n: Number of rank select bit string words since beginning
-- returns: dquote interest bits in high part and other interest bits in low part
mkDsvRawBitsByWord64s :: Word64 -> Word64 -> Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
mkDsvRawBitsByWord64s rdqs rnls rdls v = DVS.constructN (((DVS.length v + 7) `div` 8) * 2) go
  where go :: DVS.Vector Word64 -> Word64
        go u =  let vi = dvsLength u * 4 in
          if dvsLength v - vi >= 4
            then let  w0    = unsafeIndex v vi
                      w0Dqs = testWord8s (w0 .^. rdqs)
                      w0Nls = testWord8s (w0 .^. rnls)
                      w0Dls = testWord8s (w0 .^. rdls)
                      w1    = unsafeIndex v (vi + 1)
                      w1Dqs = testWord8s (w1 .^. rdqs)
                      w1Nls = testWord8s (w1 .^. rnls)
                      w1Dls = testWord8s (w1 .^. rdls)
                      w2    = unsafeIndex v (vi + 2)
                      w2Dqs = testWord8s (w2 .^. rdqs)
                      w2Nls = testWord8s (w2 .^. rnls)
                      w2Dls = testWord8s (w2 .^. rdls)
                      w3    = unsafeIndex v (vi + 3)
                      w3Dqs = testWord8s (w3 .^. rdqs)
                      w3Nls = testWord8s (w3 .^. rnls)
                      w3Dls = testWord8s (w3 .^. rdls)
                      wDqs  = (w3Dqs .<. 24) .|. (w2Dqs .<. 16) .|. (w1Dqs .<. 8) .|. w0Dqs
                      wNls  = (w3Nls .<. 24) .|. (w2Nls .<. 16) .|. (w1Nls .<. 8) .|. w0Nls
                      wDls  = (w3Dls .<. 24) .|. (w2Dls .<. 16) .|. (w1Dls .<. 8) .|. w0Dls
                  in  (comp (wDqs .<. 32) .&. 0xffffffff00000000) .|. (comp (wNls .&. wDls) .&. 0x00000000ffffffff)
            else let  w0    = atIndexOr2 0 v vi
                      w0Dqs = testWord8s (w0 .^. rdqs)
                      w0Nls = testWord8s (w0 .^. rnls)
                      w0Dls = testWord8s (w0 .^. rdls)
                      w1    = atIndexOr2 0 v (vi + 1)
                      w1Dqs = testWord8s (w1 .^. rdqs)
                      w1Nls = testWord8s (w1 .^. rnls)
                      w1Dls = testWord8s (w1 .^. rdls)
                      w2    = atIndexOr2 0 v (vi + 2)
                      w2Dqs = testWord8s (w2 .^. rdqs)
                      w2Nls = testWord8s (w2 .^. rnls)
                      w2Dls = testWord8s (w2 .^. rdls)
                      w3    = atIndexOr2 0 v (vi + 3)
                      w3Dqs = testWord8s (w3 .^. rdqs)
                      w3Nls = testWord8s (w3 .^. rnls)
                      w3Dls = testWord8s (w3 .^. rdls)
                      wDqs  = (w3Dqs .<. 24) .|. (w2Dqs .<. 16) .|. (w1Dqs .<. 8) .|. w0Dqs
                      wNls  = (w3Nls .<. 24) .|. (w2Nls .<. 16) .|. (w1Nls .<. 8) .|. w0Nls
                      wDls  = (w3Dls .<. 24) .|. (w2Dls .<. 16) .|. (w1Dls .<. 8) .|. w0Dls
                  in  (comp (wDqs .<. 32) .&. 0xffffffff00000000) .|. (comp (wNls .&. wDls) .&. 0x00000000ffffffff)

mkCummulativeDqPopCount :: DVS.Vector Word64 -> DVS.Vector Word64
mkCummulativeDqPopCount v = DVS.constructN (DVS.length v `div` 2) go
  where go :: DVS.Vector Word64 -> Word64
        go u = let  ui = dvsLength u
                    vi = ui * 2
          in if dvsLength v - vi >= 2 && vi > 0
            then  let w0 = unsafeIndex v  vi
                      w1 = unsafeIndex v (vi + 1)
                      w  = (w1 .&. 0xffffffff00000000) .|. (w0 .>. 32)
                  in unsafeIndex u (ui - 1) + fromIntegral (popCount w)
            else  let w0 = atIndexOr2 0 v  vi
                      w1 = atIndexOr2 0 v (vi + 1)
                      w  = (w1 .&. 0xffffffff00000000) .|. (w0 .>. 32)
                  in atIndexOr2 0 u (ui - 1) + fromIntegral (popCount w)

mkDsvInterestBitsByWord64sInternalXXX :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
mkDsvInterestBitsByWord64sInternalXXX rawBits cpcs v = DVS.constructN ((DVS.length v + 7) `div` 8) go
  where go :: DVS.Vector Word64 -> Word64
        go u = let ui = dvsLength u in if ui > 1
          then  let vi  = ui * 2
                    cpc = unsafeIndex cpcs (ui - 1)
                    w0  = unsafeIndex rawBits  vi
                    w1  = unsafeIndex rawBits (vi + 1)
                    w   = ((w1 .&. 0x00000000ffffffff) .<. 32) .|. ( w0 .&. 0x00000000ffffffff        )
                    d   = ( w1 .&. 0xffffffff00000000        ) .|. ((w0 .&. 0xffffffff00000000) .>. 32)
                    m   = toggle64 cpc d
                in w .&. m
          else  let vi  = fromIntegral (ui * 2)
                    cpc = atIndexOrBeforeOrLast 0 cpcs (fromIntegral (ui - 1))
                    w0  = atIndexOr 0 rawBits  vi
                    w1  = atIndexOr 0 rawBits (vi + 1)
                    w   = ((w1 .&. 0x00000000ffffffff) .<. 32) .|. ( w0 .&. 0x00000000ffffffff        )
                    d   = ( w1 .&. 0xffffffff00000000        ) .|. ((w0 .&. 0xffffffff00000000) .>. 32)
                    m   = toggle64 cpc d
                in w .&. m

mkDsvInterestBitsByWord64sXXX :: Word64 -> Word64 -> Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
mkDsvInterestBitsByWord64sXXX rdqs rnls rdls v = mkDsvInterestBitsByWord64sInternalXXX rawBits cpcs v
  where rawBits = mkDsvRawBitsByWord64s rdqs rnls rdls v
        cpcs    = mkCummulativeDqPopCount rawBits -- cummulative popcounts

mkDsvInterestBits2 :: Char -> DVS.Vector Word64 -> DVS.Vector Word64
mkDsvInterestBits2 delimiter = mkDsvInterestBitsByWord64sXXX
  CW.doubleQuote
  CW.newline
  (fillWord64WithChar8 delimiter)

-- rdqs: repeated double quotes
-- rnls: repeated new lines
-- rdls: repeated delimiters
-- numQuotes: Number of quotes since beginning
-- n: Number of rank select bit string words since beginning
-- returns: dquote interest bits in high part and other interest bits in low part
mkStripes :: Word64 -> Word64 -> Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
mkStripes rdqs rnls rdls v = DVS.constructN (((DVS.length v + 7) `div` 8) * 3) go
  where stripePatterns = DVS.fromList [rdqs, rnls, rdls]
        go :: DVS.Vector Word64 -> Word64
        go u =
          let ui = dvsLength u
              si = ui `mod` 3
              vi = (ui `div` 3) * 8
              ws = unsafeIndex stripePatterns si
          in if dvsLength v - vi >= 4
            then let  w0 = testWord8s (unsafeIndex v (vi + 0) .^. ws)
                      w1 = testWord8s (unsafeIndex v (vi + 1) .^. ws)
                      w2 = testWord8s (unsafeIndex v (vi + 2) .^. ws)
                      w3 = testWord8s (unsafeIndex v (vi + 3) .^. ws)
                      w4 = testWord8s (unsafeIndex v (vi + 4) .^. ws)
                      w5 = testWord8s (unsafeIndex v (vi + 5) .^. ws)
                      w6 = testWord8s (unsafeIndex v (vi + 6) .^. ws)
                      w7 = testWord8s (unsafeIndex v (vi + 7) .^. ws)
                      wa =  (w7 .<. 56) .|. (w6 .<. 48) .|. (w5 .<. 40) .|. (w4 .<. 32) .|.
                            (w3 .<. 24) .|. (w2 .<. 16) .|. (w1 .<.  8) .|.  w0
                  in  comp wa
            else let  w0 = testWord8s (atIndexOr2 0 v (vi + 0) .^. ws)
                      w1 = testWord8s (atIndexOr2 0 v (vi + 1) .^. ws)
                      w2 = testWord8s (atIndexOr2 0 v (vi + 2) .^. ws)
                      w3 = testWord8s (atIndexOr2 0 v (vi + 3) .^. ws)
                      w4 = testWord8s (atIndexOr2 0 v (vi + 4) .^. ws)
                      w5 = testWord8s (atIndexOr2 0 v (vi + 5) .^. ws)
                      w6 = testWord8s (atIndexOr2 0 v (vi + 6) .^. ws)
                      w7 = testWord8s (atIndexOr2 0 v (vi + 7) .^. ws)
                      wa =  (w7 .<. 56) .|. (w6 .<. 48) .|. (w5 .<. 40) .|. (w4 .<. 32) .|.
                            (w3 .<. 24) .|. (w2 .<. 16) .|. (w1 .<.  8) .|.  w0
                  in  comp wa

mkCummulativeDqPopCountFromStriped :: DVS.Vector Word64 -> DVS.Vector Word64
mkCummulativeDqPopCountFromStriped v = DVS.constructN (DVS.length v `div` 3) go
  where go :: DVS.Vector Word64 -> Word64
        go u =  let ui  = dvsLength u
                    vi  = ui * 3
                    w   = unsafeIndex v  vi
                in unsafeIndex u (ui - 1) + fromIntegral (popCount w)

mkDsvIbNlFromStriped :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
mkDsvIbNlFromStriped sv cpcs = DVS.constructN ((DVS.length sv) `div` 3) go
  where go :: DVS.Vector Word64 -> Word64
        go u = let ui = dvsLength u in if ui > 1
          then  let svi = ui * 2
                    cpc = unsafeIndex cpcs (ui - 1)
                    wdq = unsafeIndex sv  svi
                    wnl = unsafeIndex sv (svi + 1)
                    m   = toggle64 cpc wdq
                in wnl .&. m
          else  let svi = fromIntegral (ui * 2)
                    cpc = atIndexOrBeforeOrLast 0 cpcs    (fromIntegral (ui - 1))
                    wdq = atIndexOr 0 sv  svi
                    wnl = atIndexOr 0 sv (svi + 1)
                    m   = toggle64 cpc wdq
                in wnl .&. m

mkDsvIbDlFromStriped :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
mkDsvIbDlFromStriped sv cpcs = DVS.constructN ((DVS.length sv) `div` 3) go
  where go :: DVS.Vector Word64 -> Word64
        go u = let ui = dvsLength u in if ui > 1
          then  let svi = ui * 2
                    cpc = unsafeIndex cpcs (ui - 1)
                    wdq = unsafeIndex sv  svi
                    wdl = unsafeIndex sv (svi + 2)
                    m   = toggle64 cpc wdq
                in wdl .&. m
          else  let svi = fromIntegral (ui * 2)
                    cpc = atIndexOrBeforeOrLast 0 cpcs    (fromIntegral (ui - 1))
                    wdq = atIndexOr 0 sv  svi
                    wdl = atIndexOr 0 sv (svi + 2)
                    m   = toggle64 cpc wdq
                in wdl .&. m
