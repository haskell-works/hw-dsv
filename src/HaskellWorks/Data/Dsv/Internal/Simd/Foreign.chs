{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Dsv.Internal.Simd.Foreign where

import Foreign
import Foreign.C.Types
import HaskellWorks.Data.Dsv.Internal.Simd.Capabilities

#include "../cbits/simd.h"

avx2Memcpy :: Ptr CUChar -> Ptr CUChar -> CULong -> IO ()
avx2Memcpy target source len = requireAvx2 $ {#call unsafe avx2_memcpy as c_build_ibs#} target source len
{-# INLINE avx2Memcpy #-}
