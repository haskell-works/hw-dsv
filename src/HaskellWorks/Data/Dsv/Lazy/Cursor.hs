module HaskellWorks.Data.Dsv.Lazy.Cursor
  ( makeCursor
  , getDelimiter
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
import GHC.Base                                   (unsafeChr)
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

makeCursor :: Char -> LBS.ByteString -> DsvCursor
makeCursor delimiter lbs = DsvCursor
  { dsvCursorDelimiter = fromIntegral (ord delimiter)
  , dsvCursorText      = lbs
  , dsvCursorMarkers   = ib
  , dsvCursorNewlines  = nls
  , dsvCursorPosition  = 0
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

getDelimiter :: DsvCursor -> Char
getDelimiter = unsafeChr . fromIntegral . dsvCursorDelimiter
{-# INLINE getDelimiter #-}

snippet :: DsvCursor -> LBS.ByteString
snippet c = LBS.take (len `max` 0) $ LBS.drop posC $ dsvCursorText c
  where d = nextField c
        posC = fromIntegral $ dsvCursorPosition c
        posD = fromIntegral $ dsvCursorPosition d
        len  = posD - posC
{-# INLINE snippet #-}

trim :: DsvCursor -> DsvCursor
trim c = if dsvCursorPosition c >= 512
  then trim c
    { dsvCursorText     = LBS.drop 512 (dsvCursorText c)
    , dsvCursorMarkers  = drop 1 (dsvCursorMarkers c)
    , dsvCursorNewlines = drop 1 (dsvCursorNewlines c)
    , dsvCursorPosition = dsvCursorPosition c - 512
    }
  else c
{-# INLINE trim #-}

atEnd :: DsvCursor -> Bool
atEnd c = LBS.null (LBS.drop (fromIntegral (dsvCursorPosition c)) (dsvCursorText c))
{-# INLINE atEnd #-}

nextField :: DsvCursor -> DsvCursor
nextField cursor = cursor
  { dsvCursorPosition = newPos
  }
  where currentRank = rank1   (dsvCursorMarkers cursor) (dsvCursorPosition cursor)
        newPos      = select1 (dsvCursorMarkers cursor) (currentRank + 1) - 1
{-# INLINE nextField #-}

nextRow :: DsvCursor -> DsvCursor
nextRow cursor = cursor
  { dsvCursorPosition = if newPos > dsvCursorPosition cursor
                          then newPos
                          else fromIntegral (LBS.length (dsvCursorText cursor))

  }
  where currentRank = rank1   (dsvCursorNewlines cursor) (dsvCursorPosition cursor)
        newPos      = select1 (dsvCursorNewlines cursor) (currentRank + 1) - 1
{-# INLINE nextRow #-}

nextPosition :: DsvCursor -> DsvCursor
nextPosition cursor = cursor
    { dsvCursorPosition = if LBS.null (LBS.drop (fromIntegral newPos) (dsvCursorText cursor))
                            then fromIntegral (LBS.length (dsvCursorText cursor))
                            else newPos
    }
  where newPos  = dsvCursorPosition cursor + 1
{-# INLINE nextPosition #-}

getRowBetween :: DsvCursor -> DsvCursor -> DV.Vector LBS.ByteString
getRowBetween c d = DV.unfoldrN c2d go c
  where cr  = rank1 (dsvCursorMarkers c) (dsvCursorPosition c)
        dr  = rank1 (dsvCursorMarkers d) (dsvCursorPosition d)
        c2d = fromIntegral (dr - cr)
        go :: DsvCursor -> Maybe (LBS.ByteString, DsvCursor)
        go e = case nextField e of
          f -> case nextPosition f of
            g -> case snippet e of
              s -> Just (s, g)
        {-# INLINE go #-}
{-# INLINE getRowBetween #-}

toListVector :: DsvCursor -> [DV.Vector LBS.ByteString]
toListVector c = if dsvCursorPosition d > dsvCursorPosition c && not (atEnd c)
  then getRowBetween c d:toListVector (trim d)
  else []
  where d = nextPosition (nextRow c)
{-# INLINE toListVector #-}

toVectorVector :: DsvCursor -> DV.Vector (DV.Vector LBS.ByteString)
toVectorVector = DV.fromList . toListVector
{-# INLINE toVectorVector #-}
