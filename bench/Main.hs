{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main
import Data.Monoid
import Data.Word
import Foreign
import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Sv

import Control.Monad
import Data.ByteString                      (ByteString)
import Data.Vector                          (Vector)
import Data.Word
import HaskellWorks.Data.FromForeignRegion
import HaskellWorks.Data.Product
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Cursor
import HaskellWorks.Data.Sv.Load
import System.Directory
import Weigh

import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Internal            as BSI
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.Csv
import qualified Data.Vector                         as DV
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.FromForeignRegion as IO
import qualified System.IO                           as IO
import qualified System.IO.MMap                      as IO

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
  r <- fmap (Data.Csv.decode Data.Csv.HasHeader) (LBS.readFile filePath) :: IO (Either String (Vector (Vector ByteString)))
  case r of
    Left _  -> error "Unexpected parse error"
    Right v -> pure v

repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f a = a:case f a of
  Just b  -> repeatedly f b
  Nothing -> []

loadHwsv :: FilePath -> IO (Vector (Vector ByteString))
loadHwsv filePath = do
  c <- mmapDataFile2 ',' True filePath

  rows <- forM (repeatedly nextRow c) $ \row -> do
    let fieldCursors = repeatedly nextField row :: [SvCursor ByteString CsPoppy]
    let fields = DV.fromList (snippet <$> fieldCursors)

    return fields

  return (DV.fromList rows)

benchCsv :: [Benchmark]
benchCsv = let infp = "data/weigh-in.csv" in
  [ bench "cassava/decode/[ByteString]" (nfIO (loadCassava infp))
  , bench "hw-sv/decode/[ByteString]"   (nfIO (loadHwsv    infp))
  ]

main :: IO ()
main = defaultMain benchCsv
