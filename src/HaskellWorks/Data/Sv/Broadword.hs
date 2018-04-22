module HaskellWorks.Data.Sv.Broadword where

import Data.Bits.Pdep
import Data.Word
import HaskellWorks.Data.Bits.BitWise

class FillWord64 a where
  fillWord64 :: a -> Word64

instance FillWord64 Word8 where
  fillWord64 w = 0x0101010101010101 * fromIntegral w
  {-# INLINE fillWord64 #-}

toggle64 :: Word64 -> Word64 -> Word64
toggle64 carry w =
  let c = carry .&. 0x1
  in  let addend  = pdep (0x5555555555555555 .<. c) w
      in  ((addend .<. 1) .|. c) + comp w

{-
    normal case
    -----------
    00100100
    11011011

    00010000
    11011011 +

    11000111
    carry case
    -----------
    00100100
    11011011
      |  |
    10000010
    11011011 +
    00111000
-}
