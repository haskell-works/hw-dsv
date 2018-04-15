module HaskellWorks.Data.Sv.Char where

import Data.Char
import Data.Word

dQuote :: Word8
dQuote = fromIntegral (ord '"')

comma :: Word8
comma = fromIntegral (ord ',')

pipe :: Word8
pipe = fromIntegral (ord '|')

newline :: Word8
newline = fromIntegral (ord '\n')
