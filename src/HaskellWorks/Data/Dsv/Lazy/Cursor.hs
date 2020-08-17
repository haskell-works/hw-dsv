module HaskellWorks.Data.Dsv.Lazy.Cursor
  ( DsvCursor (..)
  , makeCursor
  , trim
  , atEnd
  , nextField
  , advanceField
  , nextRow
  , nextPosition
  ) where

import Data.Function
import Data.Word
import HaskellWorks.Data.Dsv.Lazy.Cursor.Internal
import HaskellWorks.Data.Dsv.Lazy.Cursor.Type
import HaskellWorks.Data.Vector.AsVector64s
import Prelude

import qualified Data.ByteString.Lazy                  as LBS
import qualified Data.Vector.Storable                  as DVS
import qualified HaskellWorks.Data.Dsv.Internal.Char   as C
import qualified HaskellWorks.Data.Dsv.Internal.Vector as DVS
import qualified HaskellWorks.Data.Simd.Comparison     as DVS

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
