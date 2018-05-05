module Main where

import Control.Monad
import Data.ByteString  (ByteString)
import Data.Vector      (Vector)
import System.Directory
import Weigh

import qualified Data.ByteString.Lazy as L
import qualified Data.Csv
import qualified System.IO            as IO

main :: IO ()
main = do
  let infp  = "data/weigh-in.csv"
  let fp    = "data/weigh-out.csv"
  exists <- doesFileExist fp
  when exists (removeFile fp)
  mainWith $ do
    setColumns [Case, Allocated, Max, Live, GCs]
    sequence_
      [ action "cassava/decode/Vector ByteString" $ do
          r <- fmap (Data.Csv.decode Data.Csv.HasHeader) (L.readFile infp) :: IO (Either String (Vector (Vector ByteString)))
          case r of
            Left _  -> error "Unexpected parse error"
            Right v -> pure v
      ]
