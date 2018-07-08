{-# LANGUAGE CPP #-}

module HaskellWorks.Data.Dsv.Internal.Simd.Capabilities where

requireAvx512 :: a -> a
requireAvx512 a = if avx512Enabled then a else error "AVX512 not enabled"
{-# INLINE requireAvx512 #-}

avx512Enabled :: Bool
#if defined(AVX512_ENABLED)
avx512Enabled = True
#else
avx512Enabled = False
#endif
{-# INLINE avx512Enabled #-}
