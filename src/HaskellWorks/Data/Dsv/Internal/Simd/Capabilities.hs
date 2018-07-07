{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Dsv.Internal.Simd.Capabilities where

requireAvx2 :: a -> a
requireAvx2 a = if avx2Enabled then a else error "AVX2 not enabled"
{-# INLINE requireAvx2 #-}

avx2Enabled :: Bool
#if defined(AVX2_ENABLED)
avx2Enabled = True
#else
avx2Enabled = False
#endif
{-# INLINE avx2Enabled #-}
