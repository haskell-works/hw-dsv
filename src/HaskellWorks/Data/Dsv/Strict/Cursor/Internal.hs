{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Dsv.Strict.Cursor.Internal where

import Control.Monad.ST
import Data.Bits                                  (popCount)
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Dsv.Internal.Bits
import HaskellWorks.Data.Dsv.Internal.Broadword
import HaskellWorks.Data.Dsv.Strict.Cursor.Type
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import Prelude

import qualified Data.Vector.Storable                       as DVS
import qualified Data.Vector.Storable.Mutable               as DVSM
import qualified HaskellWorks.Data.Dsv.Internal.Char.Word64 as CW

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

makeIndexes :: Word8 -> DVS.Vector Word64 -> (DVS.Vector Word64, DVS.Vector Word64)
makeIndexes delimiter ws = case DVS.createT $ makeIndexes' CW.doubleQuote CW.newline (fillWord64 delimiter) ws of
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
            let w0Dqs = testWord8s w0 rdqs
            let w0Nls = testWord8s w0 rnls
            let w0Dls = testWord8s w0 rdls
            let w1    = ws !!! (wsi + 1)
            let w1Dqs = testWord8s w1 rdqs
            let w1Nls = testWord8s w1 rnls
            let w1Dls = testWord8s w1 rdls
            let w2    = ws !!! (wsi + 2)
            let w2Dqs = testWord8s w2 rdqs
            let w2Nls = testWord8s w2 rnls
            let w2Dls = testWord8s w2 rdls
            let w3    = ws !!! (wsi + 3)
            let w3Dqs = testWord8s w3 rdqs
            let w3Nls = testWord8s w3 rnls
            let w3Dls = testWord8s w3 rdls
            let w4    = ws !!! (wsi + 4)
            let w4Dqs = testWord8s w4 rdqs
            let w4Nls = testWord8s w4 rnls
            let w4Dls = testWord8s w4 rdls
            let w5    = ws !!! (wsi + 5)
            let w5Dqs = testWord8s w5 rdqs
            let w5Nls = testWord8s w5 rnls
            let w5Dls = testWord8s w5 rdls
            let w6    = ws !!! (wsi + 6)
            let w6Dqs = testWord8s w6 rdqs
            let w6Nls = testWord8s w6 rnls
            let w6Dls = testWord8s w6 rdls
            let w7    = ws !!! (wsi + 7)
            let w7Dqs = testWord8s w7 rdqs
            let w7Nls = testWord8s w7 rnls
            let w7Dls = testWord8s w7 rdls
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
            let w0Dqs = testWord8s w0 rdqs
            let w0Nls = testWord8s w0 rnls
            let w0Dls = testWord8s w0 rdls
            let w1    = atIndexOr 0 ws (wsi + 1)
            let w1Dqs = testWord8s w1 rdqs
            let w1Nls = testWord8s w1 rnls
            let w1Dls = testWord8s w1 rdls
            let w2    = atIndexOr 0 ws (wsi + 2)
            let w2Dqs = testWord8s w2 rdqs
            let w2Nls = testWord8s w2 rnls
            let w2Dls = testWord8s w2 rdls
            let w3    = atIndexOr 0 ws (wsi + 3)
            let w3Dqs = testWord8s w3 rdqs
            let w3Nls = testWord8s w3 rnls
            let w3Dls = testWord8s w3 rdls
            let w4    = atIndexOr 0 ws (wsi + 4)
            let w4Dqs = testWord8s w4 rdqs
            let w4Nls = testWord8s w4 rnls
            let w4Dls = testWord8s w4 rdls
            let w5    = atIndexOr 0 ws (wsi + 5)
            let w5Dqs = testWord8s w5 rdqs
            let w5Nls = testWord8s w5 rnls
            let w5Dls = testWord8s w5 rdls
            let w6    = atIndexOr 0 ws (wsi + 6)
            let w6Dqs = testWord8s w6 rdqs
            let w6Nls = testWord8s w6 rnls
            let w6Dls = testWord8s w6 rdls
            let w7    = atIndexOr 0 ws (wsi + 7)
            let w7Dqs = testWord8s w7 rdqs
            let w7Nls = testWord8s w7 rnls
            let w7Dls = testWord8s w7 rdls
            let wDqs  = (w7Dqs  .<. 56) .|. (w6Dqs .<. 48) .|. (w5Dqs .<. 40) .|. (w4Dqs .<. 32) .|. (w3Dqs .<. 24) .|. (w2Dqs .<. 16) .|. (w1Dqs .<. 8) .|. w0Dqs
            let wNls  = (w7Nls  .<. 56) .|. (w6Nls .<. 48) .|. (w5Nls .<. 40) .|. (w4Nls .<. 32) .|. (w3Nls .<. 24) .|. (w2Nls .<. 16) .|. (w1Nls .<. 8) .|. w0Nls
            let wDls  = (w7Dls  .<. 56) .|. (w6Dls .<. 48) .|. (w5Dls .<. 40) .|. (w4Dls .<. 32) .|. (w3Dls .<. 24) .|. (w2Dls .<. 16) .|. (w1Dls .<. 8) .|. w0Dls
            let numWordQuotes = comp wDqs
            let wMask = toggle64 numQuotes numWordQuotes
            DVSM.unsafeWrite markers  (fromIntegral ui) (comp (wNls .&. wDls) .&. wMask)
            DVSM.unsafeWrite newlines (fromIntegral ui) (comp  wNls           .&. wMask)

nextCursor :: (Rank1 s, Select1 s) => DsvCursor t s -> DsvCursor t s
nextCursor cursor = cursor
  { dsvCursorPosition = newPos
  }
  where currentRank = rank1   (dsvCursorMarkers cursor) (dsvCursorPosition cursor)
        newPos      = select1 (dsvCursorMarkers cursor) (currentRank + 1)
