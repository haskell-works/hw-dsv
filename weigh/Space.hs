module Main where

import Data.ByteString (ByteString)
import Data.Vector     (Vector)
import Weigh

import qualified Data.ByteString.Lazy                as LBS
import qualified Data.Csv                            as CSV
import qualified Data.Vector                         as DV
import qualified HaskellWorks.Data.Dsv.Strict.Cursor as SVS

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f a = a:case f a of
  Just b  -> repeatedly f b
  Nothing -> []

loadCsv :: FilePath -> IO (DV.Vector (DV.Vector ByteString))
loadCsv filePath = do
  c <- SVS.mmapCursor ',' False filePath

  return $ SVS.toVectorVector c

main :: IO ()
main = do
  let infp  = "data/bench/data-0001000.csv"
  mainWith $ do
    setColumns [Case, Allocated, Max, Live, GCs]
    sequence_
      [ action "cassava/decode/Vector ByteString" $ do
          r <- fmap (CSV.decode CSV.HasHeader) (LBS.readFile infp) :: IO (Either String (Vector (Vector ByteString)))
          case r of
            Left _  -> error "Unexpected parse error"
            Right v -> pure v
      , action "hw-dsv/decode/Vector ByteString" $ do
          v <- loadCsv infp :: IO (Vector (Vector ByteString))
          pure v
      ]
  return ()
