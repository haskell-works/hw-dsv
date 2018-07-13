module HaskellWorks.Data.Dsv.Internal.Bits where

import Data.Bits.Pext
import Data.Word
import HaskellWorks.Data.Bits.BitWise

import qualified Data.Vector.Storable as DVS

_mm_cmpeq_pi8 :: Word64 -> Word64 -> Word64
_mm_cmpeq_pi8 = error "Primop support for _mm_cmpeq_pi8 needed here"

testWord8s :: Word64 -> Word64 -> Word64
testWord8s actual expected = pext (_mm_cmpeq_pi8 actual expected) 0x0101010101010101
{-# INLINE testWord8s #-}

zipOr :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
zipOr as bs = DVS.constructN (DVS.length as `max` DVS.length bs) go
  where go :: DVS.Vector Word64 -> Word64
        go u =
          let ui = DVS.length u
          in if ui < DVS.length as && ui < DVS.length bs
            then DVS.unsafeIndex as ui .|. DVS.unsafeIndex bs ui
            else error "Different sized vectors"
{-# INLINE zipOr #-}

zip2Or :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
zip2Or (a:as) (b:bs) = zipOr a b:zip2Or as bs
zip2Or (a:as) []     = a:zip2Or as []
zip2Or []     (b:bs) = b:zip2Or [] bs
zip2Or []     []     = []
{-# INLINE zip2Or #-}

zipAnd :: DVS.Vector Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
zipAnd as bs = DVS.constructN (DVS.length as `max` DVS.length bs) go
  where go :: DVS.Vector Word64 -> Word64
        go u =
          let ui = DVS.length u
          in if ui < DVS.length as && ui < DVS.length bs
            then DVS.unsafeIndex as ui .&. DVS.unsafeIndex bs ui
            else error "Different sized vectors"
{-# INLINE zipAnd #-}

zip2And :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64]
zip2And (a:as) (b:bs) = zipAnd a b:zip2And as bs
zip2And (a:as) []     = a:zip2And as []
zip2And []     (b:bs) = b:zip2And [] bs
zip2And []     []     = []
{-# INLINE zip2And #-}
