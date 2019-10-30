module HaskellWorks.Data.Dsv.Lazy.Cursor
  ( DsvCursor (..)
  , makeCursor
  , trim
  , atEnd
  , nextField
  , advanceField
  , nextRow
  , nextPosition
  -- Functions returning lazy bytestrings
  , snippet
  , toListVector
  , toVectorVector
  , selectListVector
  -- Functions returning strict bytestrings
  , getRowBetweenStrict
  , toListVectorStrict
  ) where

import Data.Function
import Data.Word
import GHC.Word                                   (Word8)
import HaskellWorks.Data.Dsv.Lazy.Cursor.Internal
import HaskellWorks.Data.Dsv.Lazy.Cursor.Type
import HaskellWorks.Data.Vector.AsVector64s
import Prelude

import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Lazy                     as LBS
import qualified Data.Vector                              as DV
import qualified Data.Vector.Storable                     as DVS
import qualified HaskellWorks.Data.Dsv.Internal.Char      as C
import qualified HaskellWorks.Data.Dsv.Internal.Vector    as DVS
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor.Lazy   as LCL
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor.Strict as LCS
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
snippet = LCL.snippet
{-# DEPRECATED snippet "Use HaskellWorks.Data.Dsv.Lazy.Cursor.Strict.snippet instead" #-}
{-# INLINE snippet #-}

toListVector :: DsvCursor -> [DV.Vector LBS.ByteString]
toListVector = LCL.toListVector
{-# DEPRECATED toListVector "Use HaskellWorks.Data.Dsv.Lazy.Cursor.toListVector instead" #-}
{-# INLINE toListVector #-}

toVectorVector :: DsvCursor -> DV.Vector (DV.Vector LBS.ByteString)
toVectorVector = LCL.toVectorVector
{-# DEPRECATED toVectorVector "Use HaskellWorks.Data.Dsv.Lazy.Cursor.toVectorVector instead" #-}
{-# INLINE toVectorVector #-}

selectListVector :: [Int] -> DsvCursor -> [[LBS.ByteString]]
selectListVector = LCL.selectListVector
{-# DEPRECATED selectListVector "Use HaskellWorks.Data.Dsv.Lazy.Cursor.selectListVector instead" #-}
{-# INLINE selectListVector #-}

toListVectorStrict :: DsvCursor -> [DV.Vector BS.ByteString]
toListVectorStrict = LCS.toListVector
{-# DEPRECATED toListVectorStrict "Use HaskellWorks.Data.Dsv.Lazy.Cursor.Strict.toListVector instead" #-}
{-# INLINE toListVectorStrict #-}

getRowBetweenStrict :: DsvCursor -> DsvCursor -> Bool -> DV.Vector BS.ByteString
getRowBetweenStrict = LCS.getRowVectorBetween
{-# INLINE getRowBetweenStrict #-}
{-# DEPRECATED getRowBetweenStrict "Use HaskellWorks.Data.Dsv.Lazy.Cursor.Strict.getRowVectorBetween instead" #-}
