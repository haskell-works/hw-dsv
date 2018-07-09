{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Dsv.Internal.Simd.Avx2.ByteString where

import Foreign.Ptr (castPtr, plusPtr)

import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Internal                    as BSI
import qualified Foreign.ForeignPtr                          as F
import qualified HaskellWorks.Data.Dsv.Internal.Simd.Foreign as F

copy :: BS.ByteString -> BS.ByteString
copy (BSI.PS x s l) = BSI.unsafeCreate l $ \p -> F.withForeignPtr x $ \f ->
  F.avx2Memcpy (castPtr p) (f `plusPtr` s) (fromIntegral l)
