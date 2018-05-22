module HaskellWorks.Data.Sv.Lazy.Cursor
  ( makeLazyCursor
  , snippet
  , countFields
  , trimCursor
  , atEnd
  , nextField
  , nextRow
  , nextPosition
  , mkRow
  , toListVector
  , toVectorVector
  ) where

import Data.Char                                 (ord)
import Data.Function
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.Sv.Bits
import HaskellWorks.Data.Sv.Lazy.Cursor.Internal
import HaskellWorks.Data.Sv.Lazy.Cursor.Type
import Prelude

import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.Vector                          as DV
import qualified HaskellWorks.Data.Sv.ByteString.Lazy as LBS
import qualified HaskellWorks.Data.Sv.Char.Word64     as CW

makeLazyCursor :: Char -> LBS.ByteString -> SvCursor
makeLazyCursor delimiter lbs = SvCursor
  { svCursorDelimiter = fromIntegral (ord delimiter)
  , svCursorText      = lbs
  , svCursorMarkers   = ib
  , svCursorNewlines  = nls
  , svCursorPosition  = 1
  }
  where ws  = LBS.toVector64Chunks 512 lbs
        ibq = makeIbs CW.doubleQuote                      <$> ws
        ibn = makeIbs CW.newline                          <$> ws
        ibd = makeIbs (CW.fillWord64WithChar8 delimiter)  <$> ws
        pcq = makeCummulativePopCount ibq
        ibr = zip2Or ibn ibd
        qm  = makeQuoteMask ibq pcq
        ib  = zip2And ibr qm
        nls = zip2And ibn qm
{-# INLINE makeLazyCursor #-}

snippet :: SvCursor -> LBS.ByteString
snippet c = LBS.take (len `max` 0) $ LBS.drop posC $ svCursorText c
  where d = nextField c
        posC = fromIntegral $ svCursorPosition c - 1
        posD = fromIntegral $ svCursorPosition d - 1
        len  = posD - posC
{-# INLINE snippet #-}

countFields :: SvCursor -> Int
countFields = go 0
  where go n d = if not (atEnd d)
          then let e = nextField d in if not (atEnd e)
            then let f = nextPosition e in go (n + 1) (trimCursor f)
            else n
          else n
        {-# INLINE go #-}
{-# INLINE countFields #-}

trimCursor :: SvCursor -> SvCursor
trimCursor c = if svCursorPosition c > 512
  then trimCursor c
    { svCursorText      = LBS.drop 512 (svCursorText c)
    , svCursorMarkers   = drop 1 (svCursorMarkers c)
    , svCursorNewlines  = drop 1 (svCursorNewlines c)
    , svCursorPosition  = svCursorPosition c - 512
    }
  else c
{-# INLINE trimCursor #-}

atEnd :: SvCursor -> Bool
atEnd c = LBS.null (LBS.drop (fromIntegral (svCursorPosition c)) (svCursorText c))
{-# INLINE atEnd #-}

nextField :: SvCursor -> SvCursor
nextField cursor = cursor
  { svCursorPosition = newPos
  }
  where currentRank = rank1   (svCursorMarkers cursor) (svCursorPosition cursor)
        newPos      = select1 (svCursorMarkers cursor) (currentRank + 1)
{-# INLINE nextField #-}

nextRow :: SvCursor -> SvCursor
nextRow cursor = cursor
  { svCursorPosition = newPos
  }
  where currentRank = rank1   (svCursorNewlines cursor) (svCursorPosition cursor)
        newPos      = select1 (svCursorNewlines cursor) (currentRank + 1)
{-# INLINE nextRow #-}

nextPosition :: SvCursor -> SvCursor
nextPosition cursor = cursor
    { svCursorPosition = if LBS.null (LBS.drop (fromIntegral newPos) (svCursorText cursor))
                            then fromIntegral (LBS.length (svCursorText cursor))
                            else newPos
    }
  where newPos  = svCursorPosition cursor + 1
{-# INLINE nextPosition #-}

mkRow :: SvCursor -> SvCursor -> DV.Vector LBS.ByteString
mkRow c d = DV.unfoldrN c2d go c
  where cr  = rank1 (svCursorMarkers c) (svCursorPosition c)
        dr  = rank1 (svCursorMarkers d) (svCursorPosition d)
        c2d = fromIntegral (dr - cr)
        go :: SvCursor -> Maybe (LBS.ByteString, SvCursor)
        go e = case nextField e of
          f -> case snippet e of
            s -> case nextPosition f of
              g -> Just (s, g)
        {-# INLINE go #-}
{-# INLINE mkRow #-}

toListVector :: SvCursor -> [DV.Vector LBS.ByteString]
toListVector c = if svCursorPosition d > svCursorPosition c && not (atEnd c)
  then mkRow c d:toListVector (trimCursor d)
  else []
  where d = nextRow c
{-# INLINE toListVector #-}

toVectorVector :: SvCursor -> DV.Vector (DV.Vector LBS.ByteString)
toVectorVector = DV.fromList . toListVector
{-# INLINE toVectorVector #-}
