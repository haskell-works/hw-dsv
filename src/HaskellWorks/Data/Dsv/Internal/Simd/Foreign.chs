{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Dsv.Internal.Simd.Foreign where

import Foreign
import HaskellWorks.Data.Dsv.Internal.Simd.Capabilities

#include "../cbits/simd.h"

type UInt8  = {#type uint8_t#}
type UInt64 = {#type uint64_t#}
type Size = {#type size_t#}

avx2Memcpy :: Ptr UInt8 -> Ptr UInt8 -> Size -> IO ()
avx2Memcpy target source len = requireAvx2 $ do
  {#call unsafe avx2_memcpy as c_build_ibs#} target source len
{-# INLINE avx2Memcpy #-}

avx2Cmpeq8 :: UInt8 -> Ptr UInt64 -> Size -> Ptr UInt8 -> IO ()
avx2Cmpeq8 byte target targetLength source = requireAvx2 $ do
  {#call unsafe avx2_cmpeq8 as c_cmpeq8#} byte target targetLength source
