{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Criterion.Main
import Data.ByteString                           (ByteString)
import Data.List
import Data.Monoid
import Data.Vector                               (Vector)
import Data.Word
import Foreign
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Product
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Char.Word64
import System.Directory
import Weigh

import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Internal             as BSI
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.Csv                             as CSV
import qualified Data.Vector                          as DV
import qualified Data.Vector.Storable                 as DVS
import qualified HaskellWorks.Data.FromForeignRegion  as IO
import qualified HaskellWorks.Data.Sv.Char.Word64     as C
import qualified HaskellWorks.Data.Sv.Lazy.Internal   as SVL
import qualified HaskellWorks.Data.Sv.Strict          as SVS
import qualified HaskellWorks.Data.Sv.Strict.Cursor   as SVS
import qualified HaskellWorks.Data.Sv.Strict.Internal as SVS
import qualified HaskellWorks.Data.Sv.Strict.Load     as SVS
import qualified System.IO                            as IO
import qualified System.IO.MMap                       as IO

setupEnvByteString :: FilePath -> IO BS.ByteString
setupEnvByteString filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- IO.mmapFileForeignPtr filepath IO.ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

sumFileByteString :: FilePath -> IO ()
sumFileByteString filePath = do
  !(bs :: BS.ByteString) <- IO.mmapFromForeignRegion filePath
  let !_ = BS.foldl' (+) 0 bs
  return ()

sumFileVectorWord64 :: FilePath -> IO ()
sumFileVectorWord64 filePath = do
  !(v :: DVS.Vector Word64) <- IO.mmapFromForeignRegion filePath
  let !_ = DVS.foldl' (+) 0 v
  return ()

benchRankJson40Conduits :: [Benchmark]
benchRankJson40Conduits =
  [ env (return ()) $ \_ -> bgroup "medium.csv"
    [ bench "Foldl' over ByteString"    (whnfIO (sumFileByteString   "corpus/medium.csv"))
    , bench "Foldl' over Vector Word64" (whnfIO (sumFileVectorWord64 "corpus/medium.csv"))
    ]
  ]

loadCassava :: FilePath -> IO (Vector (Vector ByteString))
loadCassava filePath = do
  r <- fmap (CSV.decode CSV.HasHeader) (LBS.readFile filePath) :: IO (Either String (Vector (Vector ByteString)))
  case r of
    Left _  -> error "Unexpected parse error"
    Right v -> pure v

repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f a = a:case f a of
  Just b  -> repeatedly f b
  Nothing -> []

loadHwsv :: FilePath -> IO (Vector (Vector ByteString))
loadHwsv filePath = do
  c <- SVS.mmapDataFile2 ',' True filePath

  rows <- forM (repeatedly SVS.nextRow c) $ \row -> do
    let fieldCursors = repeatedly SVS.nextField row :: [SVS.SvCursor ByteString CsPoppy]
    let fields = DV.fromList (SVS.snippet <$> fieldCursors)

    return fields

  return (DV.fromList rows)

loadHwsvFast :: FilePath -> IO (Vector (Vector ByteString))
loadHwsvFast filePath = do
  c <- SVS.mmapCursor ',' True filePath

  rows <- forM (repeatedly SVS.nextRow c) $ \row -> do
    let fieldCursors = repeatedly SVS.nextField row :: [SVS.SvCursor ByteString CsPoppy]
    let fields = DV.fromList (SVS.snippet <$> fieldCursors)

    return fields

  return (DV.fromList rows)

loadHwsvIndex :: FilePath -> IO (Vector (Vector ByteString))
loadHwsvIndex filePath = do
  !c <- SVS.mmapCursor ',' True filePath

  return DV.empty

loadHwsvFaster :: FilePath -> IO (Vector (Vector ByteString))
loadHwsvFaster filePath = do
  bsss <- SVS.loadDsv ',' True filePath

  rows <- forM bsss $ \bss -> do
    let fields = DV.fromList bss

    return fields

  return (DV.fromList rows)

loadHwsvCount :: FilePath -> IO Int
loadHwsvCount filePath = do
  !c <- SVS.mmapCursor ',' True filePath

  return (SVS.countNexts c)

loadHwsvLazy :: FilePath -> IO (Vector (Vector LBS.ByteString))
loadHwsvLazy filePath = do
  !bs <- LBS.readFile filePath

  let c = SVL.makeLazyCursor ',' bs

  return (SVL.toVectorVector c)

loadHwsvFake :: FilePath -> IO (Vector (Vector ByteString))
loadHwsvFake filePath = do
  bs <- BS.readFile filePath

  let rawRows = DV.unfoldrN ((BS.length bs + 79) `div` 80) (go 80) bs
  let rows = DV.map (\bs -> DV.unfoldrN ((BS.length bs + 4) `div` 5) (go 80) bs) rawRows

  return rows

  where go :: Int -> ByteString -> Maybe (ByteString, ByteString)
        go n bs = case BS.splitAt 80 bs of
          (as, cs) | BS.length as > n -> Just (as, cs)
          (as, cs) | BS.length cs > n -> Nothing

makeBenchCsv :: IO [Benchmark]
makeBenchCsv = do
  entries <- listDirectory "data/bench"
  let files = ("data/bench/" ++) <$> (".csv" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return $ mempty
    <> [bench ("cassava/decode/"            <> file) (nfIO (loadCassava     file))]

    -- <> [bench ("hw-sv/decode/via-list/"     <> file) (nfIO (loadHwsv      file))]
    -- <> [bench ("hw-sv/decode/via-vector/"   <> file) (nfIO (loadHwsvFast    file))]
    -- <> [bench ("hw-sv/decode/via-faster/"   <> file) (nfIO (loadHwsvFaster  file))]
    -- <> [bench ("hw-sv/decode/via-index/"    <> file) (nfIO (loadHwsvIndex   file))]
    -- <> [bench ("hw-sv/decode/via-count/"    <> file) (nfIO (loadHwsvCount   file))]

    <> [bench ("hw-sv/decode/via-lazy/"     <> file) (nfIO (loadHwsvLazy   file))]
    -- <> [bench ("hw-sv/decode/via-fake/"     <> file) (nfIO (loadHwsvFake    file))]
  return (join benchmarks)

makeBenchW64s :: IO [Benchmark]
makeBenchW64s = do
  entries <- listDirectory "data/bench"
  let files = ("data/bench/" ++) <$> (".csv" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (IO.mmapFromForeignRegion file) $ \v -> bgroup "Creating bit index from mmaped file" $ mempty
      <> [bench ("mkDsvInterestBitsByWord64sXXX with sum" <> file) (whnf (DVS.foldl (+) 0 . SVS.mkDsvInterestBitsByWord64sXXX doubleQuote newline comma) v)]
    ]
  return (join benchmarks)

makeBenchMkInterestBits :: IO [Benchmark]
makeBenchMkInterestBits = do
  entries <- listDirectory "data/bench"
  let files = ("data/bench/" ++) <$> (".csv" `isSuffixOf`) `filter` entries
  benchmarks <- forM files $ \file -> return
    [ env (IO.mmapFromForeignRegion file) $ \(v :: DVS.Vector Word64) -> bgroup "Loading lazy byte string into Word64s" $ mempty
      <> [bench ("mkDsvInterestBitsByWord64sXXX with sum" <> file) (whnf (DVS.foldr (+) 0 . SVS.mkDsvInterestBitsByWord64sXXX C.doubleQuote C.newline C.pipe) v)]
      <> [bench ("mkDsvInterestBitsByWord64s    with sum" <> file) (whnf (DVS.foldr (+) 0 . SVS.mkDsvInterestBits    '|') v)]
    ]
  return (join benchmarks)

main :: IO ()
main = do
  benchmarks <- (mconcat <$>) $ sequence $ mempty
    <> [makeBenchCsv]
    -- <> [makeBenchW64s]
    -- <> [makeBenchMkInterestBits]
  defaultMain benchmarks
