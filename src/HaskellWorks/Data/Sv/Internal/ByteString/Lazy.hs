module HaskellWorks.Data.Sv.Internal.ByteString.Lazy where

import Data.Word

import qualified Data.ByteString.Lazy                     as LBS
import qualified Data.Vector.Storable                     as DVS
import qualified HaskellWorks.Data.Sv.Internal.ByteString as BS

toVector64Chunks :: Int -> LBS.ByteString -> [DVS.Vector Word64]
toVector64Chunks chunkBytes = go
  where go :: LBS.ByteString -> [DVS.Vector Word64]
        go lbs = case LBS.splitAt (fromIntegral chunkBytes) lbs of
          (lcs, lds) -> if LBS.length lcs > 0
            then case LBS.toStrict lcs of
              cs -> BS.toVector64 cs:go lds
            else []
