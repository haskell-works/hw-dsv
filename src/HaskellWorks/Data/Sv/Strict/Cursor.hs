{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Sv.Strict.Cursor
  ( SvCursor(..)
  , SvCursor2(..)
  , SvMode(..)
  , LazyCursor(..)
  , LazyCursorType(..)
  , next
  , snippet
  , nextField
  , nextInterestingBit
  , wordAt
  , nextPosition
  , nextRow
  , lazyNext
  , lazySnippet
  ) where

import Control.Lens
import Data.Word
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Char
import HaskellWorks.Data.Sv.Strict.Cursor.Type

import qualified Data.ByteString                         as BS
import qualified Data.ByteString.Lazy                    as LBS
import qualified HaskellWorks.Data.AtIndex               as VL
import qualified HaskellWorks.Data.Sv.Char               as C
import qualified HaskellWorks.Data.Sv.Strict.Cursor.Lens as L

next :: (Rank1 s, Select1 s) => SvCursor t s -> SvCursor t s
next cursor = cursor
  { svCursorPosition = newPos
  }
  where currentRank = rank1   (svCursorMarkers cursor) (svCursorPosition cursor)
        newPos      = select1 (svCursorMarkers cursor) (currentRank + 1)

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
  where d = next c
        posC = fromIntegral $ svCursorPosition c
        posD = fromIntegral $ svCursorPosition d
        len  = posD - posC - 1

data LazyCursorType = LazyField | LazyRow deriving (Eq, Show)

lazyWordAt :: LazyCursor -> Word8
lazyWordAt c = (c ^. L.text) `LBS.index` fromIntegral ((c ^. L.position) - 1)

lazyNext :: LazyCursor -> Maybe (LazyCursorType, LazyCursor)
lazyNext cursor = ((lazyTrim <$>) <$>) . discriminate $ cursor
  { lazyCursorPosition = newPos
  }
  where currentRank     = rank1   (cursor ^. L.interestBits) (cursor ^. L.position)
        newPos          = select1 (cursor ^. L.interestBits) (currentRank + 1)
        discriminate ic = if LBS.null (LBS.drop (fromIntegral ((ic ^. L.position) - 1)) (ic ^. L.text))
          then Nothing
          else let w = lazyWordAt ic in if
            | w == ic ^. L.delimiter  -> Just (LazyField, lazyNextPosition ic)
            | w == C.newline          -> Just (LazyRow  , lazyNextPosition ic)
            | otherwise -> error "Oops"

lazyNextPosition :: LazyCursor -> LazyCursor
lazyNextPosition c = c & L.position %~ (+1)

lazyTrim :: LazyCursor -> LazyCursor
lazyTrim c
  | c ^. L.position > 128 = c & L.position      %~ (\c' -> c' - 64)
                              & L.interestBits  %~ drop 1
                              & L.text          %~ LBS.drop 64
  | otherwise            = c

lazySnippet :: LazyCursor -> LBS.ByteString
lazySnippet c = case snd <$> lazyNext c of
  Nothing ->  LBS.drop posC $ c ^. L.text
  Just d  ->  let posD = fromIntegral $ (d ^. L.position) - 1
                  len  = posD - posC - 1
              in LBS.take (len `max` 0) $ LBS.drop posC $ c ^. L.text
  where posC = fromIntegral $ (c ^. L.position) - 1
