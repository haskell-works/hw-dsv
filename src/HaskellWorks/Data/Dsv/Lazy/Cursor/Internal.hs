{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Data.Dsv.Lazy.Cursor.Internal
  ( makeIbs
  , makeCummulativePopCount
  , makeQuoteMask
  ) where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Dsv.Internal.Bits
import HaskellWorks.Data.Dsv.Internal.Broadword
import HaskellWorks.Data.Dsv.Internal.Simd.Capabilities
import Prelude

import qualified Data.Vector.Storable                            as DVS
import qualified HaskellWorks.Data.Dsv.Internal.Simd.Avx2.Vector as DVS

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

makeIbs :: Word8 -> DVS.Vector Word64 -> DVS.Vector Word64
makeIbs = if avx2Enabled then DVS.cmpeq8s else makeIbsBasic
{-# INLINE makeIbs #-}

makeIbsBasic :: Word8 -> DVS.Vector Word64 -> DVS.Vector Word64
makeIbsBasic w8 v = DVS.constructN ((DVS.length v + 7) `div` 8) go
  where iw = fillWord64 w8
        go :: DVS.Vector Word64 -> Word64
        go u = let ui = end u in
          if ui * 8 + 8 < end v
            then  let vi  = ui * 8
                      w0  = testWord8s (v !!! (vi + 0)) iw
                      w1  = testWord8s (v !!! (vi + 1)) iw
                      w2  = testWord8s (v !!! (vi + 2)) iw
                      w3  = testWord8s (v !!! (vi + 3)) iw
                      w4  = testWord8s (v !!! (vi + 4)) iw
                      w5  = testWord8s (v !!! (vi + 5)) iw
                      w6  = testWord8s (v !!! (vi + 6)) iw
                      w7  = testWord8s (v !!! (vi + 7)) iw
                      w   = (w7 .<. 56) .|.
                            (w6 .<. 48) .|.
                            (w5 .<. 40) .|.
                            (w4 .<. 32) .|.
                            (w3 .<. 24) .|.
                            (w2 .<. 16) .|.
                            (w1 .<.  8) .|.
                             w0
                  in comp w
            else  let vi  = ui * 8
                      w0  = testWord8s (atIndexOr 0 v (vi + 0)) iw
                      w1  = testWord8s (atIndexOr 0 v (vi + 1)) iw
                      w2  = testWord8s (atIndexOr 0 v (vi + 2)) iw
                      w3  = testWord8s (atIndexOr 0 v (vi + 3)) iw
                      w4  = testWord8s (atIndexOr 0 v (vi + 4)) iw
                      w5  = testWord8s (atIndexOr 0 v (vi + 5)) iw
                      w6  = testWord8s (atIndexOr 0 v (vi + 6)) iw
                      w7  = testWord8s (atIndexOr 0 v (vi + 7)) iw
                      w   = (w7 .<. 56) .|.
                            (w6 .<. 48) .|.
                            (w5 .<. 40) .|.
                            (w4 .<. 32) .|.
                            (w3 .<. 24) .|.
                            (w2 .<. 16) .|.
                            (w1 .<.  8) .|.
                            w0
                  in comp w
{-# INLINE makeIbsBasic #-}

makeQuoteMask1 :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
makeQuoteMask1 ibv pcv = DVS.constructN (DVS.length ibv) go
  where go :: DVS.Vector Word64 -> Word64
        go u =
          let ui = end u in
          toggle64 (pcv !!! ui) (ibv !!! ui)
{-# INLINE makeQuoteMask1 #-}

makeQuoteMask :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
makeQuoteMask (ibv:ibvs) (pcv:pcvs) = makeQuoteMask1 ibv pcv:makeQuoteMask ibvs pcvs
makeQuoteMask _ _                   = []
{-# INLINE makeQuoteMask #-}

makeCummulativePopCount2 :: Word64 -> DVS.Vector Word64 -> (DVS.Vector Word64, Word64)
makeCummulativePopCount2 c v = let r = DVS.constructN (DVS.length v + 1) go in (DVS.unsafeInit r, DVS.unsafeLast r)
  where go :: DVS.Vector Word64 -> Word64
        go u = let ui = end u in
          if ui > 0
            then popCount1 (v !!! (ui - 1)) + (u !!! (ui - 1))
            else c
{-# INLINE makeCummulativePopCount2 #-}

makeCummulativePopCount :: [DVS.Vector Word64] -> [DVS.Vector Word64]
makeCummulativePopCount = go 0
  where go :: Word64 -> [DVS.Vector Word64] -> [DVS.Vector Word64]
        go c (v:vs) = let (u, c') = makeCummulativePopCount2 c v in u:go c' vs
        go _ []     = []
{-# INLINE makeCummulativePopCount #-}
