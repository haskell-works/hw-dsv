module HaskellWorks.Data.Dsv.Lazy.Cursor
  ( DsvCursor (..)
  , makeCursor
  , snippet
  , trim
  , atEnd
  , nextField
  , advanceField
  , nextRow
  , nextPosition
  , toListVector
  , toVectorVector
  , selectListVector
  , getRowBetweenStrict
  , toListVectorStrict
  ) where

import Data.Function
import Data.Word
import GHC.Word                                   (Word8)
import HaskellWorks.Data.Dsv.Lazy.Cursor.Internal
import HaskellWorks.Data.Dsv.Lazy.Cursor.Type
import HaskellWorks.Data.Positioning
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.Vector.AsVector64s
import Prelude

import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Lazy                     as LBS
import qualified Data.Vector                              as DV
import qualified Data.Vector.Storable                     as DVS
import qualified HaskellWorks.Data.Dsv.Internal.Char      as C
import qualified HaskellWorks.Data.Dsv.Internal.Vector    as DVS
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor.Strict as STRICT
import qualified HaskellWorks.Data.Simd.Comparison        as DVS

makeIndexes :: [DVS.Vector Word64] -> [DVS.Vector Word64] -> [DVS.Vector Word64] -> ([DVS.Vector Word64], [DVS.Vector Word64])
makeIndexes ds ns qs = unzip $ go 0 0 ds ns qs
  where go pc carry (dv:dvs) (nv:nvs) (qv:qvs) =
          let (dv', nv', pc', carry') = DVS.indexCsvChunk pc carry dv nv qv in
          (dv', nv'):go pc' carry' dvs nvs qvs
        go _ _ [] [] [] = [(DVS.empty64, DVS.empty64)]
        go _ _ _ _ _ = error "Unbalanced inputs"

makeCursor :: Word8 -> LBS.ByteString -> DsvCursor
makeCursor delimiter lbs = DsvCursor
  { dsvCursorText      = lbs
  , dsvCursorMarkers   = ib
  , dsvCursorNewlines  = nls
  , dsvCursorPosition  = 0
  }
  where ws  = asVector64s 64 lbs
        ibq = DVS.cmpEqWord8s C.doubleQuote <$> ws
        ibn = DVS.cmpEqWord8s C.newline     <$> ws
        ibd = DVS.cmpEqWord8s delimiter     <$> ws
        (ib, nls) = makeIndexes ibd ibn ibq
{-# INLINE makeCursor #-}

snippet :: DsvCursor -> LBS.ByteString
snippet c = LBS.take (len `max` 0) $ LBS.drop posC $ dsvCursorText c
  where d = nextField c
        posC = fromIntegral $ dsvCursorPosition c
        posD = fromIntegral $ dsvCursorPosition d
        len  = posD - posC
{-# INLINE snippet #-}

advanceField :: Count -> DsvCursor -> DsvCursor
advanceField n cursor = cursor
  { dsvCursorPosition = newPos
  }
  where currentRank = rank1   (dsvCursorMarkers cursor) (dsvCursorPosition cursor)
        newPos      = select1 (dsvCursorMarkers cursor) (currentRank + n) - 1
{-# INLINE advanceField #-}

getRowBetween :: DsvCursor -> DsvCursor -> Bool -> DV.Vector LBS.ByteString
getRowBetween c d dEnd = DV.unfoldrN fields go c
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
{-# INLINE getRowBetween #-}

toListVector :: DsvCursor -> [DV.Vector LBS.ByteString]
toListVector c = if dsvCursorPosition d > dsvCursorPosition c && not (atEnd c)
  then getRowBetween c d dEnd:toListVector (trim d)
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

toListVectorStrict :: DsvCursor -> [DV.Vector BS.ByteString]
toListVectorStrict = STRICT.toListVector
{-# DEPRECATED toListVectorStrict "Use HaskellWorks.Data.Dsv.Lazy.Cursor.Strict.toListVector instead" #-}
{-# INLINE toListVectorStrict #-}

getRowBetweenStrict :: DsvCursor -> DsvCursor -> Bool -> DV.Vector BS.ByteString
getRowBetweenStrict = STRICT.getRowBetween
{-# INLINE getRowBetweenStrict #-}
{-# DEPRECATED getRowBetweenStrict "Use HaskellWorks.Data.Dsv.Lazy.Cursor.Strict.getRowBetween instead" #-}
