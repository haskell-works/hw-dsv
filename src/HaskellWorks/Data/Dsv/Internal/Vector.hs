{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Dsv.Internal.Vector
  ( empty64
  , constructNS
  , ltWord
  , indexCsvChunk
  , oddsMask
  ) where

import Control.Monad.ST
import Data.Bits.Pdep
import Data.Word
import Foreign.Storable                          (Storable)
import GHC.Int
import GHC.Prim
import GHC.Word                                  hiding (ltWord)
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Positioning

import qualified Data.Vector.Storable         as DVS
import qualified Data.Vector.Storable.Mutable as DVSM

empty64 :: DVS.Vector Word64
empty64 = DVS.replicate 64 0
{-# NOINLINE empty64 #-}

constructNS :: forall a s. Storable a => Int -> s -> (s -> DVS.Vector a -> (s, a)) -> (s, DVS.Vector a)
constructNS n s f = DVS.createT (go 0 s)
  where go :: forall q. Int -> s -> ST q (s, DVS.MVector q a)
        go n1 s1 = do
          mv :: DVS.MVector q a <- DVSM.unsafeNew n
          u <- DVS.unsafeFreeze mv
          let (s2, w) = f s1 (DVS.take n1 u)
          DVSM.unsafeWrite mv n1 w
          return (s2, mv)
{-# INLINE constructNS #-}

ltWord :: Word64 -> Word64 -> Word64
ltWord (W64# a#) (W64# b#) = fromIntegral (I64# (ltWord# a# b#))
{-# INLINE ltWord #-}

indexCsvChunk ::
     Count
  -> Word64
  -> DVS.Vector Word64
  -> DVS.Vector Word64
  -> DVS.Vector Word64
  -> (DVS.Vector Word64, DVS.Vector Word64, Word64, Word64)
indexCsvChunk qqCount qqCarry mks nls qqs = runST $ do
  tmks <- DVSM.unsafeNew len
  tnls <- DVSM.unsafeNew len
  (newCount, newCarry) <- go 0 qqCount qqCarry tmks tnls
  rmks <- DVS.unsafeFreeze tmks
  rnls <- DVS.unsafeFreeze tnls
  return (rmks, rnls, newCount, newCarry)
  where len = DVS.length mks
        go :: Int -> Word64 -> Word64 -> DVSM.MVector z Word64 -> DVSM.MVector z Word64 -> ST z (Count, Word64)
        go i pc carry tmks tnls | i < len = do
          let qq = DVS.unsafeIndex qqs i
          let mk = DVS.unsafeIndex mks i
          let nl = DVS.unsafeIndex nls i

          let enters = pdep (oddsMask .<. (0x1 .&.      pc)) qq
          let leaves = pdep (oddsMask .<. (0x1 .&. comp pc)) qq

          let compLeaves    = comp leaves
          let preQuoteMask  = enters + compLeaves
          let quoteMask     = preQuoteMask + carry
          let newCarry      = quoteMask `ltWord` (enters .|. compLeaves .|. carry)

          DVSM.unsafeWrite tmks i ((nl .|. mk) .&. quoteMask)
          DVSM.unsafeWrite tnls i ( nl         .&. quoteMask)

          go (i + 1) (popCount1 qq + pc) newCarry tmks tnls
        go _ pc carry _ _ = return (pc, carry)
{-# INLINE indexCsvChunk #-}

oddsMask :: Word64
oddsMask = 0x5555555555555555
{-# INLINE oddsMask #-}
