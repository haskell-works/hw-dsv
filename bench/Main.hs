{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.ByteString                            (ByteString)
import Data.List
import Data.Monoid
import Data.Vector                                (Vector)
import Data.Word
import Foreign
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Dsv.Internal.Char.Word64
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Product
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import System.Directory
import Weigh

import qualified Data.ByteString                                        as BS
import qualified Data.ByteString.Internal                               as BSI
import qualified Data.ByteString.Lazy                                   as LBS
import qualified Data.Csv                                               as CSV
import qualified Data.Csv.Streaming                                     as CSS
import qualified Data.Foldable                                          as F
import qualified Data.Vector                                            as DV
import qualified Data.Vector.Storable                                   as DVS
import qualified HaskellWorks.Data.Dsv.Internal.Char.Word64             as C
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor                      as SVL
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor.Type                 as SVL
import qualified HaskellWorks.Data.Dsv.Strict.Cursor                    as SVS
import qualified HaskellWorks.Data.Dsv.Strict.Cursor.Internal           as SVS
import qualified HaskellWorks.Data.Dsv.Strict.Cursor.Internal.Reference as SVS
import qualified HaskellWorks.Data.FromForeignRegion                    as IO
import qualified System.IO                                              as IO
import qualified System.IO.MMap                                         as IO

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

loadHwsvStrictIndex :: FilePath -> IO (Vector (Vector ByteString))
loadHwsvStrictIndex filePath = do
  !c <- SVS.mmapCursor ',' True filePath

  return DV.empty

loadHwsvStrict :: FilePath -> IO (Vector (Vector ByteString))
loadHwsvStrict filePath = SVS.toVectorVector <$> SVS.mmapCursor ',' True filePath

loadHwsvLazyIndex :: FilePath -> IO (Vector (Vector ByteString))
loadHwsvLazyIndex filePath = do
  !bs <- LBS.readFile filePath

  let c = SVL.makeCursor ',' bs
  let zipIndexes = zip (SVL.dsvCursorMarkers c) (SVL.dsvCursorNewlines c)
  let !n = length zipIndexes

  return DV.empty

loadHwsvLazy :: FilePath -> IO [Vector LBS.ByteString]
loadHwsvLazy filePath = do
  !bs <- LBS.readFile filePath

  let c = SVL.makeCursor ',' bs

  return (SVL.toListVector c)

makeBenchCsv :: IO [Benchmark]
makeBenchCsv = do
  entries <- listDirectory "data/bench"
  let files = ("data/bench/" ++) <$> (".csv" `isSuffixOf`) `filter` entries
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
  let files = ("data/bench/" ++) <$> (".csv" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (IO.mmapFromForeignRegion file) $ \v -> bgroup "Creating bit index from mmaped file" $ mempty
      <> [bench ("mkIbVector                  with sum" <> file) (whnf (DVS.foldl (+) 0 . SVS.mkIbVector ',') v)]
    ]
  return (join benchmarks)

makeBenchMkInterestBits :: IO [Benchmark]
makeBenchMkInterestBits = do
  entries <- listDirectory "data/bench"
  let files = ("data/bench/" ++) <$> (".csv" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (IO.mmapFromForeignRegion file) $ \(v :: DVS.Vector Word64) -> bgroup "Loading lazy byte string into Word64s" $ mempty
      <> [bench ("mkIbVector                  with sum" <> file) (whnf (DVS.foldr (+) 0 . SVS.mkIbVector        '|') v)]
      <> [bench ("makeIndexes                 with sum" <> file) (whnf (DVS.foldr (+) 0 . fst . SVS.makeIndexes '|') v)]
    ]
  return (join benchmarks)

main :: IO ()
main = do
  benchmarks <- (mconcat <$>) $ sequence $ mempty
    <> [makeBenchCsv]
    <> [makeBenchW64s]
    <> [makeBenchMkInterestBits]
  defaultMain benchmarks
