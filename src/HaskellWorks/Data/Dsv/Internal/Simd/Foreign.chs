{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Dsv.Internal.Simd.Foreign where

import Foreign
import Foreign.C.Types
import HaskellWorks.Data.Dsv.Internal.Simd.Capabilities

#include "../cbits/simd.h"

avx512Memcpy :: Ptr CUChar -> Ptr CUChar -> CULong -> IO ()
avx512Memcpy target source len = requireAvx512 $ {#call unsafe avx512_memcpy as c_build_ibs#} target source len
{-# INLINE avx512Memcpy #-}
