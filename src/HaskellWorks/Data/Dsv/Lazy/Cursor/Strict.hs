{-|
Module      : HaskellWorks.Data.Dsv.Lazy.Cursor.Strict
Description : Extraction functions that yields strict bytestrings
-}
module HaskellWorks.Data.Dsv.Lazy.Cursor.Strict
  ( snippet
  , getRowListBetween
  , getRowVectorBetween
  , toListList
  , toListVector
  , toVectorVector
  ) where

import Data.Function
import HaskellWorks.Data.Dsv.Lazy.Cursor.Internal
import HaskellWorks.Data.Dsv.Lazy.Cursor.Type
import HaskellWorks.Data.RankSelect.Base.Rank1
import Prelude

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List            as L
import qualified Data.Vector          as DV

{- HLINT ignore "Reduce duplication" -}

snippet :: DsvCursor -> Int -> BS.ByteString -> BS.ByteString
snippet c offset bs = BS.take (len `max` 0) $ BS.drop posC bs
  where d = nextField c
        posC = fromIntegral (dsvCursorPosition c) - offset
        posD = fromIntegral (dsvCursorPosition d) - offset
        len  = posD - posC
{-# INLINE snippet #-}

getRowListBetween :: DsvCursor -> DsvCursor -> Bool -> [BS.ByteString]
getRowListBetween c d dEnd = take fields (L.unfoldr go c)
  where bsA = fromIntegral $ dsvCursorPosition c
        bsZ = fromIntegral $ dsvCursorPosition d
        bsT = dsvCursorText c
        bs  = LBS.toStrict $ LBS.take (bsZ - bsA) (LBS.drop bsA bsT)

        cr  = rank1 (dsvCursorMarkers c) (dsvCursorPosition c)
        dr  = rank1 (dsvCursorMarkers d) (dsvCursorPosition d)
        c2d = fromIntegral (dr - cr)
        fields = if dEnd then c2d +1 else c2d
        go :: DsvCursor -> Maybe (BS.ByteString, DsvCursor)
        go e = case nextField e of
          f -> case nextPosition f of
            g -> case snippet e (fromIntegral bsA) bs of
              s -> Just (s, g)
        {-# INLINE go #-}
{-# INLINE getRowListBetween #-}

getRowVectorBetween :: DsvCursor -> DsvCursor -> Bool -> DV.Vector BS.ByteString
getRowVectorBetween c d dEnd = DV.unfoldrN fields go c
  where bsA = fromIntegral $ dsvCursorPosition c
        bsZ = fromIntegral $ dsvCursorPosition d
        bsT = dsvCursorText c
        bs  = LBS.toStrict $ LBS.take (bsZ - bsA) (LBS.drop bsA bsT)

        cr  = rank1 (dsvCursorMarkers c) (dsvCursorPosition c)
        dr  = rank1 (dsvCursorMarkers d) (dsvCursorPosition d)
        c2d = fromIntegral (dr - cr)
        fields = if dEnd then c2d +1 else c2d
        go :: DsvCursor -> Maybe (BS.ByteString, DsvCursor)
        go e = case nextField e of
          f -> case nextPosition f of
            g -> case snippet e (fromIntegral bsA) bs of
              s -> Just (s, g)
        {-# INLINE go #-}
{-# INLINE getRowVectorBetween #-}

toListList :: DsvCursor -> [[BS.ByteString]]
toListList c = if dsvCursorPosition d > dsvCursorPosition c && not (atEnd c)
  then getRowListBetween c d dEnd:toListList (trim d)
  else []
  where nr = nextRow c
        d = nextPosition nr
        dEnd = atEnd nr
{-# INLINE toListList #-}

toListVector :: DsvCursor -> [DV.Vector BS.ByteString]
toListVector c = if dsvCursorPosition d > dsvCursorPosition c && not (atEnd c)
  then getRowVectorBetween c d dEnd:toListVector (trim d)
  else []
  where nr = nextRow c
        d = nextPosition nr
        dEnd = atEnd nr
{-# INLINE toListVector #-}

toVectorVector :: DsvCursor -> DV.Vector (DV.Vector BS.ByteString)
toVectorVector = DV.fromList . toListVector
{-# INLINE toVectorVector #-}
