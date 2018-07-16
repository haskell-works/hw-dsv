{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module HaskellWorks.Data.Dsv.Lazy.Cursor.Internal
  ( makeCummulativePopCount
  , makeQuoteMask
  ) where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Dsv.Internal.Broadword
import Prelude

import qualified Data.Vector.Storable as DVS

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

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
