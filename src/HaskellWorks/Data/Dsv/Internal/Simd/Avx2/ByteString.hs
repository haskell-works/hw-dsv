{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Dsv.Internal.Simd.Avx2.ByteString where

import Data.Word
import Foreign.Ptr (castPtr, plusPtr)

import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Internal                    as BSI
import qualified Data.Vector.Storable                        as DVS
import qualified Foreign.ForeignPtr                          as F
import qualified Foreign.Marshal.Unsafe                      as F
import qualified Foreign.Ptr                                 as F
import qualified HaskellWorks.Data.Dsv.Internal.Simd.Foreign as F

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

copy :: BS.ByteString -> BS.ByteString
copy (BSI.PS x s l) = BSI.unsafeCreate l $ \p -> F.withForeignPtr x $ \f ->
  F.avx2Memcpy (castPtr p) (f `plusPtr` s) (fromIntegral l)

cmpeq :: Word8 -> BS.ByteString -> DVS.Vector Word64
cmpeq w8 (BSI.PS srcFptr bsOffset bsLength) = if disalignment == 0
  then F.unsafeLocalState $ do
    targetFptr <- F.mallocForeignPtrBytes bsLength
    F.withForeignPtr srcFptr $ \srcPtr -> do
      F.withForeignPtr targetFptr $ \targetPtr -> do
        _ <- F.avx2Cmpeq8
          (fromIntegral w8)
          (F.castPtr targetPtr `F.plusPtr` bsOffset)
          (fromIntegral w64sLen)
          (F.castPtr srcPtr)
        return $ DVS.unsafeFromForeignPtr targetFptr 0 w64sLen
  else error $ "Unaligned byte string: " <> show disalignment
  where w64sLen       = bsLength `div` 64
        disalignment  = bsLength - w64sLen * 64
