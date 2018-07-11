module HaskellWorks.Data.Dsv.Internal.Simd.Avx2.Vector where

import Data.Monoid ((<>))
import Data.Word

import qualified Data.Vector.Storable                        as DVS
import qualified Foreign.ForeignPtr                          as F
import qualified Foreign.Marshal.Unsafe                      as F
import qualified Foreign.Ptr                                 as F
import qualified HaskellWorks.Data.Dsv.Internal.Simd.Foreign as F

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

cmpeq8s :: Word8 -> DVS.Vector Word64 -> DVS.Vector Word64
cmpeq8s w8 v = case DVS.unsafeCast v :: DVS.Vector Word8 of
  u -> case DVS.unsafeToForeignPtr u of
    (srcFptr, srcOffset, srcLength) -> if disalignment == 0
      then F.unsafeLocalState $ do
        targetFptr <- F.mallocForeignPtrBytes srcLength
        F.withForeignPtr srcFptr $ \srcPtr -> do
          F.withForeignPtr targetFptr $ \targetPtr -> do
            _ <- F.avx2Cmpeq8
              (fromIntegral w8)
              (F.castPtr targetPtr `F.plusPtr` srcOffset)
              (fromIntegral w64sLen)
              (F.castPtr srcPtr)
            return $ DVS.unsafeFromForeignPtr targetFptr 0 w64sLen
      else error $ "Unaligned byte string: " <> show disalignment
      where w64sLen       = srcLength `div` 64
            disalignment  = srcLength - w64sLen * 64
