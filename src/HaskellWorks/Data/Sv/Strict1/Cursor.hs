{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Sv.Strict1.Cursor
  ( SvCursor(..)
  , snippet
  , nextField
  , nextInterestingBit
  , wordAt
  , nextPosition
  , nextRow
  , mmapCursor
  , countFields
  ) where

import Control.Lens
import Data.Char                                 (ord)
import Data.Word
import HaskellWorks.Data.Product
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Internal.Char
import HaskellWorks.Data.Sv.Strict1.Cursor.Type

import qualified Data.ByteString                              as BS
import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.AtIndex                    as VL
import qualified HaskellWorks.Data.FromForeignRegion          as IO
import qualified HaskellWorks.Data.Sv.Strict1.Cursor.Internal as SVS
import qualified HaskellWorks.Data.Sv.Strict1.Cursor.Lens     as L

nextInterestingBit :: (Rank1 s, Select1 s) => SvCursor t s -> SvCursor t s
nextInterestingBit cursor = cursor
  { svCursorPosition = newPos
  }
  where currentRank = rank1   (svCursorMarkers cursor) (svCursorPosition cursor)
        newPos      = select1 (svCursorMarkers cursor) (currentRank + 1) - 1

nextPosition :: SvCursor BS.ByteString s -> Maybe (SvCursor BS.ByteString s)
nextPosition cursor = if newPos < fromIntegral (VL.length (svCursorText cursor))
  then Just cursor
    { svCursorPosition = newPos
    }
  else Nothing
  where newPos = (svCursorPosition cursor + 1) `min` fromIntegral (cursor ^. L.text & BS.length)

nextField :: (Rank1 s, Select1 s) => SvCursor BS.ByteString s -> Maybe (SvCursor BS.ByteString s)
nextField c = do
  let ibCursor = nextInterestingBit c
  ibWord <- wordAt ibCursor
  if ibWord == c ^. L.delimiter
    then nextPosition ibCursor
    else Nothing

nextRow :: (Rank1 s, Select1 s) => SvCursor BS.ByteString s -> Maybe (SvCursor BS.ByteString s)
nextRow c = do
  let !ibCursor = nextInterestingBit c
  !ibWord <- wordAt ibCursor
  !newCursor <- nextPosition ibCursor
  if ibWord == newline
    then do
      Just newCursor
    else if atEnd newCursor
      then do
        Nothing
      else do
        nextRow newCursor

atEnd :: SvCursor BS.ByteString s -> Bool
atEnd c = c ^. L.position >= fromIntegral (BS.length (c ^. L.text))

wordAt :: SvCursor BS.ByteString s -> Maybe Word8
wordAt c = if pos < fromIntegral (BS.length txt)
  then Just (txt VL.!!! fromIntegral pos)
  else Nothing
  where pos = c ^. L.position
        txt = c ^. L.text

snippet :: SvCursor BS.ByteString CsPoppy -> BS.ByteString
snippet c = BS.take (len `max` 0) $ BS.drop posC $ svCursorText c
  where d = SVS.nextCursor c
        posC = fromIntegral $ svCursorPosition c
        posD = fromIntegral $ svCursorPosition d
        len  = posD - posC - 1

mmapCursor :: Char -> Bool -> FilePath -> IO (SvCursor BS.ByteString CsPoppy)
mmapCursor delimiter createIndex filePath = do
  (!bs) :*: (!v) <- IO.mmapFromForeignRegion filePath
  let !_ = v :: DVS.Vector Word64
  !ibIndex <- makeCsPoppy <$> if createIndex
    then return $ SVS.mkIbVector delimiter v
    else IO.mmapFromForeignRegion (filePath ++ ".ib")
  return SvCursor
    { svCursorDelimiter = fromIntegral (ord delimiter)
    , svCursorText      = bs
    , svCursorMarkers   = ibIndex
    , svCursorPosition  = 0
    }

countFields :: forall s. (Rank1 s, Select1 s) => SvCursor BS.ByteString s -> Int
countFields = go 0
  where go n d = case nextInterestingBit d of
          e -> case nextPosition e of
            Just f  -> go (n + 1) f
            Nothing -> n
