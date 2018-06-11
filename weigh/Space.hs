module Main where

import Data.ByteString (ByteString)
import Data.Vector     (Vector)
import Weigh

import qualified Data.ByteString.Lazy                as LBS
import qualified Data.Csv                            as CSV
import qualified Data.Csv.Streaming                  as CSS
import qualified Data.Foldable                       as F
import qualified Data.Vector                         as DV
import qualified HaskellWorks.Data.Dsv.Strict.Cursor as SVS
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor   as SVL

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f a = a:case f a of
  Just b  -> repeatedly f b
  Nothing -> []

loadCsvStrict :: FilePath -> IO (DV.Vector (DV.Vector ByteString))
loadCsvStrict filePath = do
  c <- SVS.mmapCursor ',' False filePath

  return $ SVS.toVectorVector c

loadCsvLazy :: FilePath -> IO (DV.Vector (DV.Vector LBS.ByteString))
loadCsvLazy filePath = do
  bs <- LBS.readFile filePath
  let c = SVL.makeCursor ',' bs

  return $ SVL.toVectorVector c

main :: IO ()
main = do
  let infp  = "data/bench/data-0001000.csv"
  mainWith $ do
    setColumns [Case, Allocated, Max, Live, GCs]
    sequence_
      [ action "cassava/decode/strict/Vector ByteString" $ do
          r <- fmap (CSV.decode CSV.HasHeader) (LBS.readFile infp) :: IO (Either String (Vector (Vector ByteString)))
          case r of
            Left _  -> error "Unexpected parse error"
            Right v -> pure v
      , action "cassava/decode/streaming/Vector ByteString" $ do
          fmap (DV.fromList . F.toList . CSS.decode CSS.HasHeader) (LBS.readFile infp) :: IO (Vector (Vector ByteString))
      , action "hw-dsv/decode/strict/Vector ByteString" $ do
          v <- loadCsvStrict infp :: IO (Vector (Vector ByteString))
          pure v
      , action "hw-dsv/decode/lazy/Vector ByteString" $ do
          v <- loadCsvLazy infp :: IO (Vector (Vector LBS.ByteString))
          pure v
      ]
  return ()
