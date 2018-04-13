module HaskellWorks.Data.Sv
  ( SvCursor(..)
  ) where

import Data.Monoid
import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning

import qualified Data.ByteString as BS
import qualified Data.Char       as C
import qualified Data.Vector     as DVS
import qualified System.IO       as IO

{-# ANN module ("HLint: ignore Redundant guard"        :: String) #-}

data SvCursor t s = SvCursor
  { svCursorText         :: !t
  , svCursorInterestBits :: !s
  , svCursorRank         :: !Count
  } deriving (Eq, Show)

data SvMode = SvUnquoted | SvQuoted deriving (Eq, Show)

dQuote :: Word8
dQuote = fromIntegral (C.ord '"')

comma :: Word8
comma = fromIntegral (C.ord ',')

newline :: Word8
newline = fromIntegral (C.ord '\n')

toInterestBitsVector :: BS.ByteString -> DVS.Vector Word64
toInterestBitsVector bs = DVS.unfoldrN vLen (go 64 0) (toInterestBits SvUnquoted bs)
  where vLen = (BS.length bs `div` 64) + 1
        go :: Int -> Word64 -> [Bool] -> Maybe (Word64, [Bool])
        go 0 w bs         = Just (w, bs)
        go n w []         = Just (w, [])
        go n w (True :bs) = go (n - 1) (1 .|. (w .>. 1)) bs
        go n w (False:bs) = go (n - 1) (      (w .>. 1)) bs

toInterestBits :: SvMode -> BS.ByteString -> [Bool]
toInterestBits mode text = case BS.uncons text of
  Just (a, as) -> case mode of
    SvUnquoted | a == dQuote  -> False:toInterestBits SvQuoted   as
    SvUnquoted | a == comma   -> True :toInterestBits SvUnquoted as
    SvUnquoted | a == newline -> True :toInterestBits SvUnquoted as
    SvQuoted   | a == dQuote  -> False:toInterestBits SvUnquoted as
    SvQuoted   | True         -> False:toInterestBits SvQuoted   as
  _            -> []

loadFileWithNewIndex :: FilePath -> IO (SvCursor BS.ByteString (DVS.Vector Word64))
loadFileWithNewIndex filePath = do
  text <- BS.readFile filePath
  return SvCursor
    { svCursorText          = text
    , svCursorInterestBits  = toInterestBitsVector text
    , svCursorRank          = 1
    }
