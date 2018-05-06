module HaskellWorks.Data.Sv.Vector.Storable where

import Data.Word

import qualified Data.Vector.Storable as DVS

atIndexOr :: Word64 -> DVS.Vector Word64 -> Int -> Word64
atIndexOr d v i
  | i >= 0 && i < DVS.length v  = DVS.unsafeIndex v i
  | otherwise                   = d
{-# NOINLINE atIndexOr #-}
