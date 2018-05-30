module HaskellWorks.Data.Dsv.Lazy.Cursor
  ( makeCursor
  , snippet
  , trim
  , atEnd
  , nextField
  , nextRow
  , nextPosition
  , getRowBetween
  , toListVector
  , toVectorVector
  ) where

import Data.Char                                  (ord)
import Data.Function
import HaskellWorks.Data.Dsv.Internal.Bits
import HaskellWorks.Data.Dsv.Lazy.Cursor.Internal
import HaskellWorks.Data.Dsv.Lazy.Cursor.Type
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.Vector.AsVector64s
import Prelude

import qualified Data.ByteString.Lazy                       as LBS
import qualified Data.Vector                                as DV
import qualified HaskellWorks.Data.Dsv.Internal.Char.Word64 as CW

makeCursor :: Char -> LBS.ByteString -> SvCursor
makeCursor delimiter lbs = SvCursor
  { svCursorDelimiter = fromIntegral (ord delimiter)
  , svCursorText      = lbs
  , svCursorMarkers   = ib
  , svCursorNewlines  = nls
  , svCursorPosition  = 0
  }
  where ws  = asVector64s 64 lbs
        ibq = makeIbs CW.doubleQuote                      <$> ws
        ibn = makeIbs CW.newline                          <$> ws
        ibd = makeIbs (CW.fillWord64WithChar8 delimiter)  <$> ws
        pcq = makeCummulativePopCount ibq
        ibr = zip2Or ibn ibd
        qm  = makeQuoteMask ibq pcq
        ib  = zip2And ibr qm
        nls = zip2And ibn qm
{-# INLINE makeCursor #-}

snippet :: SvCursor -> LBS.ByteString
snippet c = LBS.take (len `max` 0) $ LBS.drop posC $ svCursorText c
  where d = nextField c
        posC = fromIntegral $ svCursorPosition c
        posD = fromIntegral $ svCursorPosition d
        len  = posD - posC
{-# INLINE snippet #-}

trim :: SvCursor -> SvCursor
trim c = if svCursorPosition c > 512
  then trim c
    { svCursorText      = LBS.drop 512 (svCursorText c)
    , svCursorMarkers   = drop 1 (svCursorMarkers c)
    , svCursorNewlines  = drop 1 (svCursorNewlines c)
    , svCursorPosition  = svCursorPosition c - 512
    }
  else c
{-# INLINE trim #-}

atEnd :: SvCursor -> Bool
atEnd c = LBS.null (LBS.drop (fromIntegral (svCursorPosition c)) (svCursorText c))
{-# INLINE atEnd #-}

nextField :: SvCursor -> SvCursor
nextField cursor = cursor
  { svCursorPosition = newPos
  }
  where currentRank = rank1   (svCursorMarkers cursor) (svCursorPosition cursor)
        newPos      = select1 (svCursorMarkers cursor) (currentRank + 1) - 1
{-# INLINE nextField #-}

nextRow :: SvCursor -> SvCursor
nextRow cursor = cursor
  { svCursorPosition =  if newPos > svCursorPosition cursor
                          then newPos
                          else fromIntegral (LBS.length (svCursorText cursor))

  }
  where currentRank = rank1   (svCursorNewlines cursor) (svCursorPosition cursor)
        newPos      = select1 (svCursorNewlines cursor) (currentRank + 1) - 1
{-# INLINE nextRow #-}

nextPosition :: SvCursor -> SvCursor
nextPosition cursor = cursor
    { svCursorPosition = if LBS.null (LBS.drop (fromIntegral newPos) (svCursorText cursor))
                            then fromIntegral (LBS.length (svCursorText cursor))
                            else newPos
    }
  where newPos  = svCursorPosition cursor + 1
{-# INLINE nextPosition #-}

getRowBetween :: SvCursor -> SvCursor -> DV.Vector LBS.ByteString
getRowBetween c d = DV.unfoldrN c2d go c
  where cr  = rank1 (svCursorMarkers c) (svCursorPosition c)
        dr  = rank1 (svCursorMarkers d) (svCursorPosition d)
        c2d = fromIntegral (dr - cr)
        go :: SvCursor -> Maybe (LBS.ByteString, SvCursor)
        go e = case nextField e of
          f -> case nextPosition f of
            g -> case snippet e of
              s -> Just (s, g)
        {-# INLINE go #-}
{-# INLINE getRowBetween #-}

toListVector :: SvCursor -> [DV.Vector LBS.ByteString]
toListVector c = if svCursorPosition d > svCursorPosition c && not (atEnd c)
  then getRowBetween c d:toListVector (trim d)
  else []
  where d = nextPosition (nextRow c)
{-# INLINE toListVector #-}

toVectorVector :: SvCursor -> DV.Vector (DV.Vector LBS.ByteString)
toVectorVector = DV.fromList . toListVector
{-# INLINE toVectorVector #-}
