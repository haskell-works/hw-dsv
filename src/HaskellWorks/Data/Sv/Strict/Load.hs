{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.Data.Sv.Strict.Load
  ( SvCursor(..)
  , SvMode(..)
  , FillWord64(..)
  , loadFileWithNewIndex
  , mmapDataFile
  , mmapDataFile2
  , mkInterestBits
  , boolsToVector
  , mkDsvInterestBits
  , mmapCursor
  , loadDsv
  , countNexts
  , loadCursor2FromDsv
  , makeDv
  ) where

import Data.ByteString                           (ByteString)
import Data.Char                                 (ord)
import Data.Word
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Product
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Broadword
import HaskellWorks.Data.Sv.Strict.Cursor
import HaskellWorks.Data.Sv.Strict.Internal

import qualified Data.ByteString                     as BS
import qualified Data.Vector                         as DV
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.FromForeignRegion as IO
import qualified HaskellWorks.Data.Sv.Char           as C
import qualified HaskellWorks.Data.Sv.Char.Word64    as CW

{-# ANN module ("HLint: ignore Redundant guard"        :: String) #-}

loadFileWithNewIndex :: Word8 -> FilePath -> IO (SvCursor BS.ByteString (DVS.Vector Word64))
loadFileWithNewIndex delimiter filePath = do
  text <- BS.readFile filePath
  let ibIndex = toInterestBitsVector delimiter text
  return SvCursor
    { svCursorDelimiter     = delimiter
    , svCursorText          = text
    , svCursorInterestBits  = ibIndex
    , svCursorPosition      = 1
    , svCursorPopCount      = popCount1 ibIndex
    }

mmapDataFile :: Word8 -> Bool -> FilePath -> IO (SvCursor BS.ByteString CsPoppy)
mmapDataFile delimiter createIndex filePath = do
  !bs <- IO.mmapFromForeignRegion filePath
  !ibIndex <- makeCsPoppy <$> if createIndex
    then return $ toInterestBitsVector delimiter bs
    else IO.mmapFromForeignRegion (filePath ++ ".ib")
  return SvCursor
    { svCursorDelimiter     = delimiter
    , svCursorText          = bs
    , svCursorInterestBits  = ibIndex
    , svCursorPosition      = 0
    , svCursorPopCount      = popCount1 ibIndex
    }

mmapDataFile2 :: Char -> Bool -> FilePath -> IO (SvCursor BS.ByteString CsPoppy)
mmapDataFile2 delimiter createIndex filePath = do
  (!bs) :*: (!v) <- IO.mmapFromForeignRegion filePath
  let !_ = v :: DVS.Vector Word64
  !ibIndex <- makeCsPoppy <$> if createIndex
    then return $ mkDsvInterestBits delimiter v
    else IO.mmapFromForeignRegion (filePath ++ ".ib")
  return SvCursor
    { svCursorDelimiter     = fromIntegral (ord delimiter)
    , svCursorText          = bs
    , svCursorInterestBits  = ibIndex
    , svCursorPosition      = 0
    , svCursorPopCount      = popCount1 ibIndex
    }

mmapCursor :: Char -> Bool -> FilePath -> IO (SvCursor BS.ByteString CsPoppy)
mmapCursor delimiter createIndex filePath = do
  (!bs) :*: (!v) <- IO.mmapFromForeignRegion filePath
  let !_ = v :: DVS.Vector Word64
  !ibIndex <- makeCsPoppy <$> if createIndex
    then return $ mkDsvInterestBits2 delimiter v
    else IO.mmapFromForeignRegion (filePath ++ ".ib")
  return SvCursor
    { svCursorDelimiter     = fromIntegral (ord delimiter)
    , svCursorText          = bs
    , svCursorInterestBits  = ibIndex
    , svCursorPosition      = 0
    , svCursorPopCount      = popCount1 ibIndex
    }

extractRows :: forall s. (Rank1 s, Select1 s) => SvCursor BS.ByteString s -> [[BS.ByteString]]
extractRows = go []
  where go :: [BS.ByteString] -> SvCursor BS.ByteString s -> [[BS.ByteString]]
        go fs c = case nextInterestingBit c of
          Just ibc -> case wordAt ibc of
            Just ibw -> case nextPosition ibc of
              Just newCursor ->
                let start = fromIntegral (svCursorPosition c)
                    len   = (fromIntegral (svCursorPosition ibc) - start) `max` 0
                    text  = BS.take len $ BS.drop start $ svCursorText c
                in if ibw == C.newline
                  then reverse (text:fs):go [] newCursor
                  else go (text:fs) newCursor
              Nothing        -> [reverse fs]
            Nothing -> [reverse fs]
          Nothing -> [reverse fs]

loadDsv :: Char -> Bool -> FilePath -> IO [[ByteString]]
loadDsv delimiter createIndex filePath = extractRows <$> mmapCursor delimiter createIndex filePath

countNexts :: forall s. (Rank1 s, Select1 s) => SvCursor BS.ByteString s -> Int
countNexts = go 0
  where go n d = case nextInterestingBit d of
          Just e -> case nextPosition e of
            Just f  -> go (n + 1) f
            Nothing -> n
          Nothing -> n

loadCursor2FromDsv :: Char -> FilePath -> IO (SvCursor2 BS.ByteString CsPoppy)
loadCursor2FromDsv delimiter filePath = do
  (!bs) :*: (!v) <- IO.mmapFromForeignRegion filePath
  let !_ = v  :: DVS.Vector Word64
  let !_ = bs :: BS.ByteString
  let wdq = CW.doubleQuote
  let wnl = CW.newline
  let wdl = fillWord64WithChar8 delimiter
  let sv  = mkStripes wdq wnl wdl v
  let cv  = mkCummulativeDqPopCountFromStriped sv
  let nv  = mkDsvIbNlFromStriped sv cv
  let dv  = mkDsvIbDlFromStriped sv cv
  return SvCursor2
    { svCursor2Delimiter   = fromIntegral (ord delimiter)
    , svCursor2Text        = bs
    , svCursor2IbNewline   = makeCsPoppy nv
    , svCursor2IbDelimiter = makeCsPoppy dv
    , svCursor2Position    = 1
    }

makeDv :: SvCursor2 BS.ByteString CsPoppy -> DV.Vector (DV.Vector BS.ByteString)
makeDv c = DV.constructN rowCount makeRow
  where rowCount :: Int
        rowCount = fromIntegral (popCount1 (svCursor2IbNewline c) + 1)
        fv = svCursor2IbDelimiter c
        makeRow :: DV.Vector (DV.Vector ByteString) -> DV.Vector ByteString
        makeRow u =
          let ui = DV.length u
              makeField :: DV.Vector ByteString -> ByteString
              makeField = undefined
              fieldCount = fromIntegral (select1 fv (fromIntegral ui))
          in if ui > 0
            then DV.constructN fieldCount makeField
            else undefined

