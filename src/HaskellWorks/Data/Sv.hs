module HaskellWorks.Data.Sv
  ( SvCursor(..)
  , SvMode(..)
  , toInterestBitsVector
  , toInterestBits
  , loadFileWithNewIndex
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.Sv.Char

import qualified Data.ByteString as BS
import qualified Data.Vector     as DVS

{-# ANN module ("HLint: ignore Redundant guard"        :: String) #-}

data SvCursor t s = SvCursor
  { svCursorText         :: !t
  , svCursorInterestBits :: !s
  , svCursorRank         :: !Count
  } deriving (Eq, Show)

data SvMode = SvUnquoted | SvQuoted deriving (Eq, Show)

toInterestBitsVector :: BS.ByteString -> DVS.Vector Word64
toInterestBitsVector bs = DVS.unfoldrN vLen (go 64 0) (toInterestBits SvUnquoted bs)
  where vLen = (BS.length bs `div` 64) + 1
        go :: Int -> Word64 -> [Bool] -> Maybe (Word64, [Bool])
        go 0 w cs         = Just (w, cs)
        go _ w []         = Just (w, [])
        go n w (True :cs) = go (n - 1) (1 .|. (w .>. 1)) cs
        go n w (False:cs) = go (n - 1)        (w .>. 1)  cs

toInterestBits :: SvMode -> BS.ByteString -> [Bool]
toInterestBits mode text = case BS.uncons text of
  Just (a, as) -> case mode of
    SvUnquoted | a == dQuote  -> False:toInterestBits SvQuoted   as
    SvUnquoted | a == comma   -> True :toInterestBits SvUnquoted as
    SvUnquoted | a == newline -> True :toInterestBits SvUnquoted as
    SvUnquoted | True         -> False:toInterestBits SvUnquoted as
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
