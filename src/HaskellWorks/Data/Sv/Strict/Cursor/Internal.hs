{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Sv.Strict.Cursor.Internal where

import Control.Monad.ST
import Data.Bits                                 (popCount)
import Data.Semigroup
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.Sv.Internal.Bits
import HaskellWorks.Data.Sv.Internal.Broadword
import HaskellWorks.Data.Sv.Internal.Char.Word64
import HaskellWorks.Data.Sv.Strict.Cursor.Type
import Prelude

import qualified Data.Vector.Storable                      as DVS
import qualified Data.Vector.Storable.Mutable              as DVSM
import qualified HaskellWorks.Data.Length                  as V
import qualified HaskellWorks.Data.Sv.Internal.Char.Word64 as CW

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

makeIndexes :: Char -> DVS.Vector Word64 -> (DVS.Vector Word64, DVS.Vector Word64)
makeIndexes delimiter ws = case DVS.createT $ makeIndexes' CW.doubleQuote CW.newline (fillWord64WithChar8 delimiter) ws of
  [markers, newlines] -> (markers, newlines)
  _                   -> error "This should not happen"

makeIndexes' :: forall s.
     Word64
  -> Word64
  -> Word64
  -> DVS.Vector Word64
  -> ST s [DVS.MVector s Word64]
makeIndexes' rdqs rnls rdls ws = do
  (markers  :: DVSM.MVector s Word64) <- DVSM.unsafeNew ((DVS.length ws + 7) `div` 8)
  (newlines :: DVSM.MVector s Word64) <- DVSM.unsafeNew ((DVS.length ws + 7) `div` 8)
  go markers newlines 0 0
  return [markers, newlines]
  where go :: DVSM.MVector s Word64 -> DVSM.MVector s Word64 -> Position -> Count -> ST s ()
        go markers newlines wsi numQuotes = let ui = wsi `div` 8 in if wsi >= 0 && wsi + 8 <= end ws
          then do
            let w0    = ws !!!  wsi
            let w0Dqs = testWord8s (w0 .^. rdqs)
            let w0Nls = testWord8s (w0 .^. rnls)
            let w0Dls = testWord8s (w0 .^. rdls)
            let w1    = ws !!! (wsi + 1)
            let w1Dqs = testWord8s (w1 .^. rdqs)
            let w1Nls = testWord8s (w1 .^. rnls)
            let w1Dls = testWord8s (w1 .^. rdls)
            let w2    = ws !!! (wsi + 2)
            let w2Dqs = testWord8s (w2 .^. rdqs)
            let w2Nls = testWord8s (w2 .^. rnls)
            let w2Dls = testWord8s (w2 .^. rdls)
            let w3    = ws !!! (wsi + 3)
            let w3Dqs = testWord8s (w3 .^. rdqs)
            let w3Nls = testWord8s (w3 .^. rnls)
            let w3Dls = testWord8s (w3 .^. rdls)
            let w4    = ws !!! (wsi + 4)
            let w4Dqs = testWord8s (w4 .^. rdqs)
            let w4Nls = testWord8s (w4 .^. rnls)
            let w4Dls = testWord8s (w4 .^. rdls)
            let w5    = ws !!! (wsi + 5)
            let w5Dqs = testWord8s (w5 .^. rdqs)
            let w5Nls = testWord8s (w5 .^. rnls)
            let w5Dls = testWord8s (w5 .^. rdls)
            let w6    = ws !!! (wsi + 6)
            let w6Dqs = testWord8s (w6 .^. rdqs)
            let w6Nls = testWord8s (w6 .^. rnls)
            let w6Dls = testWord8s (w6 .^. rdls)
            let w7    = ws !!! (wsi + 7)
            let w7Dqs = testWord8s (w7 .^. rdqs)
            let w7Nls = testWord8s (w7 .^. rnls)
            let w7Dls = testWord8s (w7 .^. rdls)
            let wDqs  = (w7Dqs  .<. 56) .|. (w6Dqs .<. 48) .|. (w5Dqs .<. 40) .|. (w4Dqs .<. 32) .|. (w3Dqs .<. 24) .|. (w2Dqs .<. 16) .|. (w1Dqs .<. 8) .|. w0Dqs
            let wNls  = (w7Nls  .<. 56) .|. (w6Nls .<. 48) .|. (w5Nls .<. 40) .|. (w4Nls .<. 32) .|. (w3Nls .<. 24) .|. (w2Nls .<. 16) .|. (w1Nls .<. 8) .|. w0Nls
            let wDls  = (w7Dls  .<. 56) .|. (w6Dls .<. 48) .|. (w5Dls .<. 40) .|. (w4Dls .<. 32) .|. (w3Dls .<. 24) .|. (w2Dls .<. 16) .|. (w1Dls .<. 8) .|. w0Dls
            let numWordQuotes = comp wDqs
            let wMask = toggle64 numQuotes numWordQuotes
            let newNumQuotes = numQuotes + fromIntegral (popCount numWordQuotes)
            DVSM.unsafeWrite markers  (fromIntegral ui) (comp (wNls .&. wDls) .&. wMask)
            DVSM.unsafeWrite newlines (fromIntegral ui) (comp  wNls           .&. wMask)
            go markers newlines (wsi + 8) newNumQuotes

          else do
            let w0    = atIndexOr 0 ws  wsi
            let w0Dqs = testWord8s (w0 .^. rdqs)
            let w0Nls = testWord8s (w0 .^. rnls)
            let w0Dls = testWord8s (w0 .^. rdls)
            let w1    = atIndexOr 0 ws (wsi + 1)
            let w1Dqs = testWord8s (w1 .^. rdqs)
            let w1Nls = testWord8s (w1 .^. rnls)
            let w1Dls = testWord8s (w1 .^. rdls)
            let w2    = atIndexOr 0 ws (wsi + 2)
            let w2Dqs = testWord8s (w2 .^. rdqs)
            let w2Nls = testWord8s (w2 .^. rnls)
            let w2Dls = testWord8s (w2 .^. rdls)
            let w3    = atIndexOr 0 ws (wsi + 3)
            let w3Dqs = testWord8s (w3 .^. rdqs)
            let w3Nls = testWord8s (w3 .^. rnls)
            let w3Dls = testWord8s (w3 .^. rdls)
            let w4    = atIndexOr 0 ws (wsi + 4)
            let w4Dqs = testWord8s (w4 .^. rdqs)
            let w4Nls = testWord8s (w4 .^. rnls)
            let w4Dls = testWord8s (w4 .^. rdls)
            let w5    = atIndexOr 0 ws (wsi + 5)
            let w5Dqs = testWord8s (w5 .^. rdqs)
            let w5Nls = testWord8s (w5 .^. rnls)
            let w5Dls = testWord8s (w5 .^. rdls)
            let w6    = atIndexOr 0 ws (wsi + 6)
            let w6Dqs = testWord8s (w6 .^. rdqs)
            let w6Nls = testWord8s (w6 .^. rnls)
            let w6Dls = testWord8s (w6 .^. rdls)
            let w7    = atIndexOr 0 ws (wsi + 7)
            let w7Dqs = testWord8s (w7 .^. rdqs)
            let w7Nls = testWord8s (w7 .^. rnls)
            let w7Dls = testWord8s (w7 .^. rdls)
            let wDqs  = (w7Dqs  .<. 56) .|. (w6Dqs .<. 48) .|. (w5Dqs .<. 40) .|. (w4Dqs .<. 32) .|. (w3Dqs .<. 24) .|. (w2Dqs .<. 16) .|. (w1Dqs .<. 8) .|. w0Dqs
            let wNls  = (w7Nls  .<. 56) .|. (w6Nls .<. 48) .|. (w5Nls .<. 40) .|. (w4Nls .<. 32) .|. (w3Nls .<. 24) .|. (w2Nls .<. 16) .|. (w1Nls .<. 8) .|. w0Nls
            let wDls  = (w7Dls  .<. 56) .|. (w6Dls .<. 48) .|. (w5Dls .<. 40) .|. (w4Dls .<. 32) .|. (w3Dls .<. 24) .|. (w2Dls .<. 16) .|. (w1Dls .<. 8) .|. w0Dls
            let numWordQuotes = comp wDqs
            let wMask = toggle64 numQuotes numWordQuotes
            DVSM.unsafeWrite markers  (fromIntegral ui) (comp (wNls .&. wDls) .&. wMask)
            DVSM.unsafeWrite newlines (fromIntegral ui) (comp  wNls           .&. wMask)




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

mkIbVector' :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
mkIbVector' rawBits cpcs v = DVS.constructN ((DVS.length v + 7) `div` 8) go
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

mkIbVector :: Char -> DVS.Vector Word64 -> DVS.Vector Word64
mkIbVector delimiter v = mkIbVector' rawBits cpcs v
  where rdqs    = CW.doubleQuote
        rnls    = CW.newline
        rdls    = fillWord64WithChar8 delimiter
        rawBits = mkDsvRawBitsByWord64s rdqs rnls rdls v
        cpcs    = mkCummulativeDqPopCount rawBits -- cummulative popcounts

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

nextCursor :: (Rank1 s, Select1 s) => SvCursor t s -> SvCursor t s
nextCursor cursor = cursor
  { svCursorPosition = newPos
  }
  where currentRank = rank1   (svCursorMarkers cursor) (svCursorPosition cursor)
        newPos      = select1 (svCursorMarkers cursor) (currentRank + 1)
