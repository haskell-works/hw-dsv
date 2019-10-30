module Main where

import Data.ByteString                     (ByteString)
import Data.Vector                         (Vector)
import HaskellWorks.Data.Dsv.Internal.Char (comma)
import Weigh

import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.Csv                               as CSV
import qualified Data.Csv.Streaming                     as CSS
import qualified Data.Foldable                          as F
import qualified Data.Vector                            as DV
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor      as SVL
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor.Lazy as SVLL
import qualified HaskellWorks.Data.Dsv.Strict.Cursor    as SVS

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f a = a:case f a of
  Just b  -> repeatedly f b
  Nothing -> []

loadCsvStrict :: FilePath -> IO (DV.Vector (DV.Vector ByteString))
loadCsvStrict filePath = do
  c <- SVS.mmapCursor comma False filePath

  return $ SVS.toVectorVector c

loadCsvLazy :: FilePath -> IO [DV.Vector LBS.ByteString]
loadCsvLazy filePath = do
  bs <- LBS.readFile filePath
  let c = SVL.makeCursor comma bs

  return $ SVLL.toListVector c

main :: IO ()
main = do
  let infp  = "data/bench/data-0001000.csv"
  mainWith $ do
    setColumns [Case, Allocated, Max, Live, GCs]
    sequence_
      [ action "cassava strict" $ do
          r <- fmap (CSV.decode CSV.HasHeader) (LBS.readFile infp) :: IO (Either String (Vector (Vector ByteString)))
          case r of
            Left _  -> error "Unexpected parse error"
            Right v -> pure v
      , action "cassava streaming" $ do
          fmap (F.toList . CSS.decode CSS.HasHeader) (LBS.readFile infp) :: IO [Vector ByteString]
      , action "hw-dsv strict" $ do
          v <- loadCsvStrict infp :: IO (Vector (Vector ByteString))
          pure v
      , action "hw-dsv lazy" $ do
          v <- loadCsvLazy infp :: IO [Vector LBS.ByteString]
          pure v
      ]
  return ()
