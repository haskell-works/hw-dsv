{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.Data.Sv.Strict.Load
  ( SVS.SvCursor(..)
  , SVS.SvMode(..)
  , FillWord64(..)
  , loadFileWithNewIndex
  , mmapDataFile
  , mmapDataFile2
  , mmapCursor
  , loadDsv
  , countFields
  ) where

import Data.ByteString                           (ByteString)
import Data.Char                                 (ord)
import Data.Word
import HaskellWorks.Data.Product
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Internal.Broadword

import qualified Data.ByteString                             as BS
import qualified Data.Vector.Storable                        as DVS
import qualified HaskellWorks.Data.FromForeignRegion         as IO
import qualified HaskellWorks.Data.Sv.Internal.Char          as C
import qualified HaskellWorks.Data.Sv.Strict.Cursor          as SVS
import qualified HaskellWorks.Data.Sv.Strict.Cursor.Internal as SVS

{-# ANN module ("HLint: ignore Redundant guard"        :: String) #-}

loadFileWithNewIndex :: Char -> FilePath -> IO (SVS.SvCursor BS.ByteString (DVS.Vector Word64))
loadFileWithNewIndex delimiter filePath = do
  text <- BS.readFile filePath
  let ibIndex = SVS.mkIbVectorViaList delimiter text
  return SVS.SvCursor
    { SVS.svCursorDelimiter = fromIntegral (ord delimiter)
    , SVS.svCursorText      = text
    , SVS.svCursorMarkers   = ibIndex
    , SVS.svCursorPosition  = 1
    }

mmapDataFile :: Char -> Bool -> FilePath -> IO (SVS.SvCursor BS.ByteString CsPoppy)
mmapDataFile delimiter createIndex filePath = do
  !bs <- IO.mmapFromForeignRegion filePath
  !ibIndex <- makeCsPoppy <$> if createIndex
    then return $ SVS.mkIbVectorViaList delimiter bs
    else IO.mmapFromForeignRegion (filePath ++ ".ib")
  return SVS.SvCursor
    { SVS.svCursorDelimiter = fromIntegral (ord delimiter)
    , SVS.svCursorText      = bs
    , SVS.svCursorMarkers   = ibIndex
    , SVS.svCursorPosition  = 0
    }

mmapDataFile2 :: Char -> Bool -> FilePath -> IO (SVS.SvCursor BS.ByteString CsPoppy)
mmapDataFile2 delimiter createIndex filePath = do
  (!bs) :*: (!v) <- IO.mmapFromForeignRegion filePath
  let !_ = v :: DVS.Vector Word64
  !ibIndex <- makeCsPoppy <$> if createIndex
    then return $ SVS.mkDsvInterestBits delimiter v
    else IO.mmapFromForeignRegion (filePath ++ ".ib")
  return SVS.SvCursor
    { SVS.svCursorDelimiter = fromIntegral (ord delimiter)
    , SVS.svCursorText      = bs
    , SVS.svCursorMarkers   = ibIndex
    , SVS.svCursorPosition  = 0
    }

mmapCursor :: Char -> Bool -> FilePath -> IO (SVS.SvCursor BS.ByteString CsPoppy)
mmapCursor delimiter createIndex filePath = do
  (!bs) :*: (!v) <- IO.mmapFromForeignRegion filePath
  let !_ = v :: DVS.Vector Word64
  !ibIndex <- makeCsPoppy <$> if createIndex
    then return $ SVS.mkIbVector delimiter v
    else IO.mmapFromForeignRegion (filePath ++ ".ib")
  return SVS.SvCursor
    { SVS.svCursorDelimiter = fromIntegral (ord delimiter)
    , SVS.svCursorText      = bs
    , SVS.svCursorMarkers   = ibIndex
    , SVS.svCursorPosition  = 0
    }

extractRows :: forall s. (Rank1 s, Select1 s) => SVS.SvCursor BS.ByteString s -> [[BS.ByteString]]
extractRows = go []
  where go :: [BS.ByteString] -> SVS.SvCursor BS.ByteString s -> [[BS.ByteString]]
        go fs c = case SVS.nextInterestingBit c of
          ibc -> case SVS.wordAt ibc of
            Just ibw -> case SVS.nextPosition ibc of
              Just newCursor ->
                let start = fromIntegral (SVS.svCursorPosition c)
                    len   = (fromIntegral (SVS.svCursorPosition ibc) - start) `max` 0
                    text  = BS.take len $ BS.drop start $ SVS.svCursorText c
                in if ibw == C.newline
                  then reverse (text:fs):go [] newCursor
                  else go (text:fs) newCursor
              Nothing -> [reverse fs]
            Nothing -> [reverse fs]

loadDsv :: Char -> Bool -> FilePath -> IO [[ByteString]]
loadDsv delimiter createIndex filePath = extractRows <$> mmapCursor delimiter createIndex filePath

countFields :: forall s. (Rank1 s, Select1 s) => SVS.SvCursor BS.ByteString s -> Int
countFields = go 0
  where go n d = case SVS.nextInterestingBit d of
          e -> case SVS.nextPosition e of
            Just f  -> go (n + 1) f
            Nothing -> n
