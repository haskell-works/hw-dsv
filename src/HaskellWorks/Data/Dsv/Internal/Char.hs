module HaskellWorks.Data.Dsv.Internal.Char where

import Data.Char
import Data.Word

doubleQuote :: Word8
doubleQuote = fromIntegral (ord '"')

comma :: Word8
comma = fromIntegral (ord ',')

pipe :: Word8
pipe = fromIntegral (ord '|')

newline :: Word8
newline = fromIntegral (ord '\n')
