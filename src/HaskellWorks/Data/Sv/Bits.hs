module HaskellWorks.Data.Sv.Bits where

import Data.Bits.Pext
import Data.Word
import HaskellWorks.Data.Bits.BitWise

testWord8s :: Word64 -> Word64
testWord8s w =  let w8s = w
                    w4s = w8s .|. (w8s .>. 4)
                    w2s = w4s .|. (w4s .>. 2)
                    w1s = w2s .|. (w2s .>. 1)
                in  pext w1s 0x0101010101010101
