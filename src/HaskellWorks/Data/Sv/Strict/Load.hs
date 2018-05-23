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
import HaskellWorks.Data.Sv.Internal.Broadword

import qualified Data.ByteString                             as BS
import qualified Data.Vector                                 as DV
import qualified Data.Vector.Storable                        as DVS
import qualified HaskellWorks.Data.FromForeignRegion         as IO
import qualified HaskellWorks.Data.Sv.Internal.Char          as C
import qualified HaskellWorks.Data.Sv.Internal.Char.Word64   as CW
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
    , SVS.svCursorPopCount  = popCount1 ibIndex
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
    , SVS.svCursorPopCount  = popCount1 ibIndex
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
    , SVS.svCursorPopCount  = popCount1 ibIndex
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
    , SVS.svCursorPopCount  = popCount1 ibIndex
    }

extractRows :: forall s. (Rank1 s, Select1 s) => SVS.SvCursor BS.ByteString s -> [[BS.ByteString]]
extractRows = go []
  where go :: [BS.ByteString] -> SVS.SvCursor BS.ByteString s -> [[BS.ByteString]]
        go fs c = case SVS.nextInterestingBit c of
          Just ibc -> case SVS.wordAt ibc of
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
          Nothing -> [reverse fs]

loadDsv :: Char -> Bool -> FilePath -> IO [[ByteString]]
loadDsv delimiter createIndex filePath = extractRows <$> mmapCursor delimiter createIndex filePath

countFields :: forall s. (Rank1 s, Select1 s) => SVS.SvCursor BS.ByteString s -> Int
countFields = go 0
  where go n d = case SVS.nextInterestingBit d of
          Just e -> case SVS.nextPosition e of
            Just f  -> go (n + 1) f
            Nothing -> n
          Nothing -> n

loadCursor2FromDsv :: Char -> FilePath -> IO (SVS.SvCursor2 BS.ByteString CsPoppy)
loadCursor2FromDsv delimiter filePath = do
  (!bs) :*: (!v) <- IO.mmapFromForeignRegion filePath
  let !_ = v  :: DVS.Vector Word64
  let !_ = bs :: BS.ByteString
  let wdq = CW.doubleQuote
  let wnl = CW.newline
  let wdl = SVS.fillWord64WithChar8 delimiter
  let sv  = SVS.mkStripes wdq wnl wdl v
  let cv  = SVS.mkCummulativeDqPopCountFromStriped sv
  let nv  = SVS.mkDsvIbNlFromStriped sv cv
  let dv  = SVS.mkDsvIbDlFromStriped sv cv
  return SVS.SvCursor2
    { SVS.svCursor2Delimiter   = fromIntegral (ord delimiter)
    , SVS.svCursor2Text        = bs
    , SVS.svCursor2IbNewline   = makeCsPoppy nv
    , SVS.svCursor2IbDelimiter = makeCsPoppy dv
    , SVS.svCursor2Position    = 1
    }

makeDv :: SVS.SvCursor2 BS.ByteString CsPoppy -> DV.Vector (DV.Vector BS.ByteString)
makeDv c = DV.constructN rowCount makeRow
  where rowCount :: Int
        rowCount = fromIntegral (popCount1 (SVS.svCursor2IbNewline c) + 1)
        fv = SVS.svCursor2IbDelimiter c
        makeRow :: DV.Vector (DV.Vector ByteString) -> DV.Vector ByteString
        makeRow u =
          let ui = DV.length u
              makeField :: DV.Vector ByteString -> ByteString
              makeField = undefined
              fieldCount = fromIntegral (select1 fv (fromIntegral ui))
          in if ui > 0
            then DV.constructN fieldCount makeField
            else undefined

