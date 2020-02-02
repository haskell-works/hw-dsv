{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Dsv.Strict.Cursor.Internal.Reference where

import Data.Bits                                  (popCount)
import Data.Semigroup
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Dsv.Internal.Bits
import HaskellWorks.Data.Dsv.Internal.Broadword
import HaskellWorks.Data.Dsv.Strict.Cursor.Type
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import Prelude

import qualified Data.Vector.Storable                       as DVS
import qualified HaskellWorks.Data.Dsv.Internal.Char.Word64 as CW

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

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
mkDsvRawBitsByWord64s rdqs rnls rdls v = DVS.generate (((DVS.length v + 7) `div` 8) * 2) go
  where go :: Int -> Word64
        go u =  let vi = fromIntegral u * 4 in
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

mkIbVector' :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
mkIbVector' rawBits cpcs v = DVS.generate ((DVS.length v + 7) `div` 8) go
  where go :: Int -> Word64
        go u = let ui = fromIntegral u in if ui > 1
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

mkIbVector :: Word8 -> DVS.Vector Word64 -> DVS.Vector Word64
mkIbVector delimiter v = mkIbVector' rawBits cpcs v
  where rdqs    = CW.doubleQuote
        rnls    = CW.newline
        rdls    = fillWord64 delimiter
        rawBits = mkDsvRawBitsByWord64s rdqs rnls rdls v
        cpcs    = mkCummulativeDqPopCount rawBits -- cummulative popcounts

-- rdqs: repeated double quotes
-- rnls: repeated new lines
-- rdls: repeated delimiters
-- numQuotes: Number of quotes since beginning
-- n: Number of rank select bit string words since beginning
-- returns: dquote interest bits in high part and other interest bits in low part
mkStripes :: Word64 -> Word64 -> Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
mkStripes rdqs rnls rdls v = DVS.generate (((DVS.length v + 7) `div` 8) * 3) go
  where stripePatterns = DVS.fromList [rdqs, rnls, rdls]
        go :: Int -> Word64
        go u =
          let ui = fromIntegral u
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
mkDsvIbNlFromStriped sv cpcs = DVS.generate ((DVS.length sv) `div` 3) go
  where go :: Int -> Word64
        go u = let ui = fromIntegral u in if ui > 1
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
mkDsvIbDlFromStriped sv cpcs = DVS.generate ((DVS.length sv) `div` 3) go
  where go :: Int -> Word64
        go u = let ui = fromIntegral u in if ui > 1
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

nextCursor :: (Rank1 s, Select1 s) => DsvCursor t s -> DsvCursor t s
nextCursor cursor = cursor
  { dsvCursorPosition = newPos
  }
  where currentRank = rank1   (dsvCursorMarkers cursor) (dsvCursorPosition cursor)
        newPos      = select1 (dsvCursorMarkers cursor) (currentRank + 1)
