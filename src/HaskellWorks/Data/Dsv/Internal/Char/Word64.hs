module HaskellWorks.Data.Dsv.Internal.Char.Word64 where

import Data.Word

import qualified HaskellWorks.Data.Dsv.Internal.Char as C

doubleQuote :: Word64
doubleQuote = 0x0101010101010101 * fromIntegral C.doubleQuote

comma :: Word64
comma = 0x0101010101010101 * fromIntegral C.comma

pipe :: Word64
pipe = 0x0101010101010101 * fromIntegral C.pipe

newline :: Word64
newline = 0x0101010101010101 * fromIntegral C.newline
