module Main where

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
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.Csv
import qualified Data.Vector                         as DV
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.FromForeignRegion as IO
import qualified System.IO                           as IO

repeatedly :: (a -> Maybe a) -> a -> [a]
repeatedly f a = a:case f a of
  Just b  -> repeatedly f b
  Nothing -> []

loadCsv :: FilePath -> IO (DV.Vector (DV.Vector ByteString))
loadCsv filePath = do
  c <- mmapDataFile2 ',' True filePath

  rows <- forM (repeatedly nextRow c) $ \row -> do
    let fieldCursors = repeatedly nextField row :: [SvCursor ByteString CsPoppy]
    let fields = DV.fromList (snippet <$> fieldCursors)

    return fields

  return (DV.fromList rows)
  -- where columnToFieldString :: [SvCursor BS.ByteString CsPoppy] -> Int -> BS.ByteString
  --       columnToFieldString fields column = maybe mempty (B.byteString . snippet) (drop column fields & listToMaybe)

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
          r <- fmap (Data.Csv.decode Data.Csv.HasHeader) (LBS.readFile infp) :: IO (Either String (Vector (Vector ByteString)))
          case r of
            Left _  -> error "Unexpected parse error"
            Right v -> pure v
      , action "hw-sv/decode/Vector ByteString" $ do
          v <- loadCsv infp :: IO (Vector (Vector ByteString))
          pure v
      ]
