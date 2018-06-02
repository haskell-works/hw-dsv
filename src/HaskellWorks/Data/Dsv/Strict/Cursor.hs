{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Dsv.Strict.Cursor
  ( DsvCursor(..)
  , snippet
  , nextField
  , nextPosition
  , nextRow
  , mmapCursor
  , toListVector
  , toVectorVector
  ) where

import Data.Char                                 (ord)
import Data.Word
import HaskellWorks.Data.Dsv.Strict.Cursor.Type
import HaskellWorks.Data.Product
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy

import qualified Data.ByteString                              as BS
import qualified Data.Vector                                  as DV
import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.Dsv.Strict.Cursor.Internal as SVS
import qualified HaskellWorks.Data.FromForeignRegion          as IO

mmapCursor :: Char -> Bool -> FilePath -> IO (DsvCursor BS.ByteString CsPoppy)
mmapCursor delimiter createIndex filePath = do
  (!bs) :*: (!v) <- IO.mmapFromForeignRegion filePath
  let !_ = v :: DVS.Vector Word64
  (!markers, !newlines) <- if createIndex
    then return $ SVS.makeIndexes delimiter v
    else (,)
      <$> IO.mmapFromForeignRegion (filePath ++ ".markers.idx")
      <*> IO.mmapFromForeignRegion (filePath ++ ".newlines.idx")
  return DsvCursor
    { dsvCursorDelimiter = fromIntegral (ord delimiter)
    , dsvCursorText      = bs
    , dsvCursorMarkers   = makeCsPoppy markers
    , dsvCursorNewlines  = makeCsPoppy newlines
    , dsvCursorPosition  = 0
    }

snippet :: DsvCursor BS.ByteString CsPoppy -> BS.ByteString
snippet c = BS.take (len `max` 0) $ BS.drop posC $ dsvCursorText c
  where d = nextField c
        posC = fromIntegral $ dsvCursorPosition c
        posD = fromIntegral $ dsvCursorPosition d
        len  = posD - posC
{-# INLINE snippet #-}

atEnd :: DsvCursor BS.ByteString CsPoppy -> Bool
atEnd c = BS.null (BS.drop (fromIntegral (dsvCursorPosition c)) (dsvCursorText c))
{-# INLINE atEnd #-}

nextField :: DsvCursor BS.ByteString CsPoppy -> DsvCursor BS.ByteString CsPoppy
nextField cursor = cursor
  { dsvCursorPosition = newPos
  }
  where currentRank = rank1   (dsvCursorMarkers cursor) (dsvCursorPosition cursor)
        newPos      = select1 (dsvCursorMarkers cursor) (currentRank + 1) - 1
{-# INLINE nextField #-}

nextRow :: DsvCursor BS.ByteString CsPoppy -> DsvCursor BS.ByteString CsPoppy
nextRow cursor = cursor
  { dsvCursorPosition = if newPos > dsvCursorPosition cursor
                          then newPos
                          else fromIntegral (BS.length (dsvCursorText cursor))

  }
  where currentRank = rank1   (dsvCursorNewlines cursor) (dsvCursorPosition cursor)
        newPos      = select1 (dsvCursorNewlines cursor) (currentRank + 1) - 1
{-# INLINE nextRow #-}

nextPosition :: DsvCursor BS.ByteString CsPoppy -> DsvCursor BS.ByteString CsPoppy
nextPosition cursor = cursor
    { dsvCursorPosition = if BS.null (BS.drop (fromIntegral newPos) (dsvCursorText cursor))
                            then fromIntegral (BS.length (dsvCursorText cursor))
                            else newPos
    }
  where newPos  = dsvCursorPosition cursor + 1
{-# INLINE nextPosition #-}

getRowBetween :: DsvCursor BS.ByteString CsPoppy -> DsvCursor BS.ByteString CsPoppy -> DV.Vector BS.ByteString
getRowBetween c d = DV.unfoldrN c2d go c
  where cr  = rank1 (dsvCursorMarkers c) (dsvCursorPosition c)
        dr  = rank1 (dsvCursorMarkers d) (dsvCursorPosition d)
        c2d = fromIntegral (dr - cr)
        go :: DsvCursor BS.ByteString CsPoppy -> Maybe (BS.ByteString, DsvCursor BS.ByteString CsPoppy)
        go e = case nextField e of
          f -> case nextPosition f of
            g -> case snippet e of
              s -> Just (s, g)
        {-# INLINE go #-}
{-# INLINE getRowBetween #-}

toListVector :: DsvCursor BS.ByteString CsPoppy -> [DV.Vector BS.ByteString]
toListVector c = if dsvCursorPosition d > dsvCursorPosition c && not (atEnd c)
  then getRowBetween c d:toListVector d
  else []
  where d = nextPosition (nextRow c)
{-# INLINE toListVector #-}

toVectorVector :: DsvCursor BS.ByteString CsPoppy -> DV.Vector (DV.Vector BS.ByteString)
toVectorVector = DV.fromList . toListVector
{-# INLINE toVectorVector #-}
