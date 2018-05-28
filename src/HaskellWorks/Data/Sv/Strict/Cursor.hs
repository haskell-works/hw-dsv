{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Sv.Strict.Cursor
  ( SvCursor(..)
  , snippet
  , nextField
  , nextPosition
  , nextRow
  , mmapCursor
  , countFields
  , toListVector
  , toVectorVector
  ) where

import Data.Char                                 (ord)
import Data.Word
import HaskellWorks.Data.Product
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Strict.Cursor.Type

import qualified Data.ByteString                             as BS
import qualified Data.Vector                                 as DV
import qualified Data.Vector.Storable                        as DVS
import qualified HaskellWorks.Data.FromForeignRegion         as IO
import qualified HaskellWorks.Data.Sv.Strict.Cursor.Internal as SVS

mmapCursor :: Char -> Bool -> FilePath -> IO (SvCursor BS.ByteString CsPoppy)
mmapCursor delimiter createIndex filePath = do
  (!bs) :*: (!v) <- IO.mmapFromForeignRegion filePath
  let !_ = v :: DVS.Vector Word64
  (!markers, !newlines) <- if createIndex
    then return $ SVS.makeIndexes delimiter v
    else (,)
      <$> IO.mmapFromForeignRegion (filePath ++ ".markers.idx")
      <*> IO.mmapFromForeignRegion (filePath ++ ".newlines.idx")
  return SvCursor
    { svCursorDelimiter = fromIntegral (ord delimiter)
    , svCursorText      = bs
    , svCursorMarkers   = makeCsPoppy markers
    , svCursorNewlines  = makeCsPoppy newlines
    , svCursorPosition  = 0
    }

snippet :: SvCursor BS.ByteString CsPoppy -> BS.ByteString
snippet c = BS.take (len `max` 0) $ BS.drop posC $ svCursorText c
  where d = nextField c
        posC = fromIntegral $ svCursorPosition c
        posD = fromIntegral $ svCursorPosition d
        len  = posD - posC
{-# INLINE snippet #-}

countFields :: SvCursor BS.ByteString CsPoppy -> Int
countFields = go 0
  where go n d = if not (atEnd d)
          then let e = nextField d in if not (atEnd e)
            then let f = nextPosition e in go (n + 1) f
            else n
          else n
        {-# INLINE go #-}
{-# INLINE countFields #-}

atEnd :: SvCursor BS.ByteString CsPoppy -> Bool
atEnd c = BS.null (BS.drop (fromIntegral (svCursorPosition c)) (svCursorText c))
{-# INLINE atEnd #-}

nextField :: SvCursor BS.ByteString CsPoppy -> SvCursor BS.ByteString CsPoppy
nextField cursor = cursor
  { svCursorPosition = newPos
  }
  where currentRank = rank1   (svCursorMarkers cursor) (svCursorPosition cursor)
        newPos      = select1 (svCursorMarkers cursor) (currentRank + 1) - 1
{-# INLINE nextField #-}

nextRow :: SvCursor BS.ByteString CsPoppy -> SvCursor BS.ByteString CsPoppy
nextRow cursor = cursor
  { svCursorPosition =  if newPos > svCursorPosition cursor
                          then newPos
                          else fromIntegral (BS.length (svCursorText cursor))

  }
  where currentRank = rank1   (svCursorNewlines cursor) (svCursorPosition cursor)
        newPos      = select1 (svCursorNewlines cursor) (currentRank + 1) - 1
{-# INLINE nextRow #-}

nextPosition :: SvCursor BS.ByteString CsPoppy -> SvCursor BS.ByteString CsPoppy
nextPosition cursor = cursor
    { svCursorPosition = if BS.null (BS.drop (fromIntegral newPos) (svCursorText cursor))
                            then fromIntegral (BS.length (svCursorText cursor))
                            else newPos
    }
  where newPos  = svCursorPosition cursor + 1
{-# INLINE nextPosition #-}

getRowBetween :: SvCursor BS.ByteString CsPoppy -> SvCursor BS.ByteString CsPoppy -> DV.Vector BS.ByteString
getRowBetween c d = DV.unfoldrN c2d go c
  where cr  = rank1 (svCursorMarkers c) (svCursorPosition c)
        dr  = rank1 (svCursorMarkers d) (svCursorPosition d)
        c2d = fromIntegral (dr - cr)
        go :: SvCursor BS.ByteString CsPoppy -> Maybe (BS.ByteString, SvCursor BS.ByteString CsPoppy)
        go e = case nextField e of
          f -> case nextPosition f of
            g -> case snippet e of
              s -> Just (s, g)
        {-# INLINE go #-}
{-# INLINE getRowBetween #-}

toListVector :: SvCursor BS.ByteString CsPoppy -> [DV.Vector BS.ByteString]
toListVector c = if svCursorPosition d > svCursorPosition c && not (atEnd c)
  then getRowBetween c d:toListVector d
  else []
  where d = nextPosition (nextRow c)
{-# INLINE toListVector #-}

toVectorVector :: SvCursor BS.ByteString CsPoppy -> DV.Vector (DV.Vector BS.ByteString)
toVectorVector = DV.fromList . toListVector
{-# INLINE toVectorVector #-}
