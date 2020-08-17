{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.ByteString                     (ByteString)
import Data.Vector                         (Vector)
import Data.Word
import HaskellWorks.Data.Dsv.Internal.Char (comma, pipe)
import System.Directory

import qualified Data.ByteString.Lazy                                   as LBS
import qualified Data.Csv                                               as CSV
import qualified Data.Csv.Streaming                                     as CSS
import qualified Data.List                                              as L
import qualified Data.Vector.Storable                                   as DVS
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor                      as SVL
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor.Lazy                 as SVLL
import qualified HaskellWorks.Data.Dsv.Strict.Cursor                    as SVS
import qualified HaskellWorks.Data.Dsv.Strict.Cursor.Internal           as SVS
import qualified HaskellWorks.Data.Dsv.Strict.Cursor.Internal.Reference as SVS
import qualified HaskellWorks.Data.FromForeignRegion                    as IO
import qualified HaskellWorks.Data.RankSelect.CsPoppy                   as RS

{- HLINT ignore "Reduce duplication"  -}

loadCassavaStrict :: FilePath -> IO (Vector (Vector ByteString))
loadCassavaStrict filePath = do
  !bs <- LBS.readFile filePath
  let r = CSV.decode CSS.HasHeader bs :: Either String (Vector (Vector ByteString))
  case r of
    Left _  -> error "Unexpected parse error"
    Right v -> pure v

loadCassavaStreaming :: FilePath -> IO (CSS.Records (Vector ByteString))
loadCassavaStreaming filePath = do
  !bs <- LBS.readFile filePath
  let r = CSS.decode CSV.HasHeader bs :: CSS.Records (Vector ByteString)
  pure r

loadHwsvStrictIndex :: FilePath -> IO (SVS.DsvCursor ByteString RS.CsPoppy)
loadHwsvStrictIndex = SVS.mmapCursor comma True

loadHwsvStrict :: FilePath -> IO (Vector (Vector ByteString))
loadHwsvStrict filePath = SVS.toVectorVector <$> SVS.mmapCursor comma True filePath

loadHwsvLazyIndex :: FilePath -> IO [(DVS.Vector Word64, DVS.Vector Word64)]
loadHwsvLazyIndex filePath = do
  !bs <- LBS.readFile filePath

  let c = SVL.makeCursor comma bs
  pure (zip (SVL.dsvCursorMarkers c) (SVL.dsvCursorNewlines c))

loadHwsvLazy :: FilePath -> IO [Vector LBS.ByteString]
loadHwsvLazy filePath = do
  !bs <- LBS.readFile filePath

  let c = SVL.makeCursor comma bs

  pure (SVLL.toListVector c)

makeBenchCsv :: IO [Benchmark]
makeBenchCsv = do
  entries <- listDirectory "data/bench"
  let files = ("data/bench/" ++) <$> (".csv" `L.isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return $ mempty
    <> [bench ("cassava/decode/via-strict/"       <> file) (nfIO (loadCassavaStrict    file))]
    <> [bench ("cassava/decode/via-streaming/"    <> file) (nfIO (loadCassavaStreaming file))]
    <> [bench ("hw-dsv/decode/via-strict/"        <> file) (nfIO (loadHwsvStrict       file))]
    <> [bench ("hw-dsv/decode/via-lazy/"          <> file) (nfIO (loadHwsvLazy         file))]
    <> [bench ("hw-dsv/decode/via-strict/index/"  <> file) (nfIO (loadHwsvStrictIndex  file))]
    <> [bench ("hw-dsv/decode/via-lazy/index/"    <> file) (nfIO (loadHwsvLazyIndex    file))]
  return (join benchmarks)

makeBenchW64s :: IO [Benchmark]
makeBenchW64s = do
  entries <- listDirectory "data/bench"
  let files = ("data/bench/" ++) <$> (".csv" `L.isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (IO.mmapFromForeignRegion file) $ \v -> bgroup "Creating bit index from mmaped file" $ mempty
      <> [bench ("mkIbVector                  with sum" <> file) (whnf (DVS.foldl (+) 0 . SVS.mkIbVector comma) v)]
    ]
  return (join benchmarks)

makeBenchMkInterestBits :: IO [Benchmark]
makeBenchMkInterestBits = do
  entries <- listDirectory "data/bench"
  let files = ("data/bench/" ++) <$> (".csv" `L.isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (IO.mmapFromForeignRegion file) $ \(v :: DVS.Vector Word64) -> bgroup "Loading lazy byte string into Word64s" $ mempty
      <> [bench ("mkIbVector                  with sum" <> file) (whnf (DVS.foldr (+) 0 . SVS.mkIbVector        pipe) v)]
      <> [bench ("makeIndexes                 with sum" <> file) (whnf (DVS.foldr (+) 0 . fst . SVS.makeIndexes pipe) v)]
    ]
  return (join benchmarks)

main :: IO ()
main = do
  benchmarks <- (mconcat <$>) $ sequence $ mempty
    <> [makeBenchCsv]
    -- <> [makeBenchW64s]
    -- <> [makeBenchMkInterestBits]
  defaultMain benchmarks
