{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Sv.Strict.Cursor
  ( SvCursor(..)
  , SvCursor2(..)
  , SvMode(..)
  , snippet
  , nextField
  , nextInterestingBit
  , wordAt
  , nextPosition
  , nextRow
  , toVectorVector
  ) where

import Control.Lens
import Data.Word
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Internal.Char
import HaskellWorks.Data.Sv.Strict.Cursor.Type

import qualified Data.ByteString                             as BS
import qualified Data.Vector                                 as DV
import qualified HaskellWorks.Data.AtIndex                   as VL
import qualified HaskellWorks.Data.Sv.Strict.Cursor.Internal as SVS
import qualified HaskellWorks.Data.Sv.Strict.Cursor.Lens     as L
import qualified HaskellWorks.Data.Sv.Strict.Cursor.Type     as SVS

nextInterestingBit :: (Rank1 s, Select1 s) => SvCursor t s -> Maybe (SvCursor t s)
nextInterestingBit cursor = if currentRank < cursor ^. L.popCount
  then Just cursor
    { svCursorPosition = newPos
    }
  else Nothing
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
  ibCursor <- nextInterestingBit c
  ibWord <- wordAt ibCursor
  if ibWord == c ^. L.delimiter
    then nextPosition ibCursor
    else Nothing

nextRow :: (Rank1 s, Select1 s) => SvCursor BS.ByteString s -> Maybe (SvCursor BS.ByteString s)
nextRow c = do
  !ibCursor <- nextInterestingBit c
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

toVectorVector :: SVS.SvCursor2 BS.ByteString CsPoppy -> DV.Vector (DV.Vector BS.ByteString)
toVectorVector c = DV.constructN rowCount makeRow
  where rowCount :: Int
        rowCount = fromIntegral (popCount1 (SVS.svCursor2IbNewline c) + 1)
        fv = SVS.svCursor2IbDelimiter c
        makeRow :: DV.Vector (DV.Vector BS.ByteString) -> DV.Vector BS.ByteString
        makeRow u =
          let ui = DV.length u
              makeField :: DV.Vector BS.ByteString -> BS.ByteString
              makeField = undefined
              fieldCount = fromIntegral (select1 fv (fromIntegral ui))
          in if ui > 0
            then DV.constructN fieldCount makeField
            else undefined
