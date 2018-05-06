module HaskellWorks.Data.Sv.ByteString where

import Data.Word
import HaskellWorks.Data.Bits.BitWise

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.Vector.Storable   as DVS

toVector64 :: BS.ByteString -> DVS.Vector Word64
toVector64 bs = DVS.constructN ((BS.length bs + 7) `div` 8) go
  where go :: DVS.Vector Word64 -> Word64
        go u = let ui = DVS.length u in
          if ui * 8 + 8 < BS.length bs
            then  let vi = ui * 8
                      w0 = fromIntegral (BS.unsafeIndex bs (vi + 0)) :: Word64
                      w1 = fromIntegral (BS.unsafeIndex bs (vi + 1)) :: Word64
                      w2 = fromIntegral (BS.unsafeIndex bs (vi + 2)) :: Word64
                      w3 = fromIntegral (BS.unsafeIndex bs (vi + 3)) :: Word64
                      w4 = fromIntegral (BS.unsafeIndex bs (vi + 4)) :: Word64
                      w5 = fromIntegral (BS.unsafeIndex bs (vi + 5)) :: Word64
                      w6 = fromIntegral (BS.unsafeIndex bs (vi + 6)) :: Word64
                      w7 = fromIntegral (BS.unsafeIndex bs (vi + 7)) :: Word64
                  in  (w0 .<.  0) .|.
                      (w1 .<.  8) .|.
                      (w2 .<. 16) .|.
                      (w3 .<. 24) .|.
                      (w4 .<. 32) .|.
                      (w5 .<. 40) .|.
                      (w6 .<. 48) .|.
                      (w7 .<. 56)
            else  let vi = ui * 8
                      w0 = fromIntegral (indexOrZero bs (vi + 0)) :: Word64
                      w1 = fromIntegral (indexOrZero bs (vi + 1)) :: Word64
                      w2 = fromIntegral (indexOrZero bs (vi + 2)) :: Word64
                      w3 = fromIntegral (indexOrZero bs (vi + 3)) :: Word64
                      w4 = fromIntegral (indexOrZero bs (vi + 4)) :: Word64
                      w5 = fromIntegral (indexOrZero bs (vi + 5)) :: Word64
                      w6 = fromIntegral (indexOrZero bs (vi + 6)) :: Word64
                      w7 = fromIntegral (indexOrZero bs (vi + 7)) :: Word64
                  in  (w0 .<.  0) .|.
                      (w1 .<.  8) .|.
                      (w2 .<. 16) .|.
                      (w3 .<. 24) .|.
                      (w4 .<. 32) .|.
                      (w5 .<. 40) .|.
                      (w6 .<. 48) .|.
                      (w7 .<. 56)

indexOrZero :: BS.ByteString -> Int -> Word8
indexOrZero bs i = if i >= 0 && i < BS.length bs
  then BS.unsafeIndex bs i
  else 0
