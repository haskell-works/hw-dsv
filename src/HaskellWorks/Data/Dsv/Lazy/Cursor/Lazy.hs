{-|
Module      : HaskellWorks.Data.Dsv.Lazy.Cursor.Strict
Description : Extraction functions that yields lazy bytestrings
-}
module HaskellWorks.Data.Dsv.Lazy.Cursor.Lazy
  ( snippet
  , getRowListBetween
  , getRowVectorBetween
  , toListList
  , toListVector
  , toVectorVector
  , selectListVector
  ) where

import Data.Function
import HaskellWorks.Data.Dsv.Lazy.Cursor.Internal
import HaskellWorks.Data.Dsv.Lazy.Cursor.Type
import HaskellWorks.Data.RankSelect.Base.Rank1
import Prelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.List            as L
import qualified Data.Vector          as DV

snippet :: DsvCursor -> LBS.ByteString
snippet c = LBS.take (len `max` 0) $ LBS.drop posC $ dsvCursorText c
  where d = nextField c
        posC = fromIntegral $ dsvCursorPosition c
        posD = fromIntegral $ dsvCursorPosition d
        len  = posD - posC
{-# INLINE snippet #-}

getRowVectorBetween :: DsvCursor -> DsvCursor -> Bool -> DV.Vector LBS.ByteString
getRowVectorBetween c d dEnd = DV.unfoldrN fields go c
  where cr  = rank1 (dsvCursorMarkers c) (dsvCursorPosition c)
        dr  = rank1 (dsvCursorMarkers d) (dsvCursorPosition d)
        c2d = fromIntegral (dr - cr)
        fields = if dEnd then c2d +1 else c2d
        go :: DsvCursor -> Maybe (LBS.ByteString, DsvCursor)
        go e = case nextField e of
          f -> case nextPosition f of
            g -> case snippet e of
              s -> Just (s, g)
        {-# INLINE go #-}
{-# INLINE getRowVectorBetween #-}

getRowListBetween :: DsvCursor -> DsvCursor -> Bool -> [LBS.ByteString]
getRowListBetween c d dEnd = take fields (L.unfoldr go c)
  where cr  = rank1 (dsvCursorMarkers c) (dsvCursorPosition c)
        dr  = rank1 (dsvCursorMarkers d) (dsvCursorPosition d)
        c2d = fromIntegral (dr - cr)
        fields = if dEnd then c2d +1 else c2d
        go :: DsvCursor -> Maybe (LBS.ByteString, DsvCursor)
        go e = case nextField e of
          f -> case nextPosition f of
            g -> case snippet e of
              s -> Just (s, g)
        {-# INLINE go #-}
{-# INLINE getRowListBetween #-}

toListList :: DsvCursor -> [[LBS.ByteString]]
toListList c = if dsvCursorPosition d > dsvCursorPosition c && not (atEnd c)
  then getRowListBetween c d dEnd:toListList (trim d)
  else []
  where nr = nextRow c
        d = nextPosition nr
        dEnd = atEnd nr
{-# INLINE toListList #-}

toListVector :: DsvCursor -> [DV.Vector LBS.ByteString]
toListVector c = if dsvCursorPosition d > dsvCursorPosition c && not (atEnd c)
  then getRowVectorBetween c d dEnd:toListVector (trim d)
  else []
  where nr = nextRow c
        d = nextPosition nr
        dEnd = atEnd nr
{-# INLINE toListVector #-}

toVectorVector :: DsvCursor -> DV.Vector (DV.Vector LBS.ByteString)
toVectorVector = DV.fromList . toListVector
{-# INLINE toVectorVector #-}

selectRowFrom :: [Int] -> DsvCursor -> [LBS.ByteString]
selectRowFrom sel c = go <$> sel
  where go :: Int -> LBS.ByteString
        go n = snippet nc
          where nc = nextPosition (advanceField (fromIntegral n) c)
        {-# INLINE go #-}
{-# INLINE selectRowFrom #-}

selectListVector :: [Int] -> DsvCursor -> [[LBS.ByteString]]
selectListVector sel c = if dsvCursorPosition d > dsvCursorPosition c && not (atEnd c)
  then selectRowFrom sel c:selectListVector sel (trim d)
  else []
  where nr = nextRow c
        d = nextPosition nr
{-# INLINE selectListVector #-}
