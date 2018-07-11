module HaskellWorks.Data.Dsv.Internal.ByteString
  ( toByteString
  , rechunkAlignedAt
  , rechunkPaddedAlignedAt
  , cmpeq8s
  ) where

import Data.Monoid                         ((<>))
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Dsv.Internal.Bits
import HaskellWorks.Data.Vector.AsVector64

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.Vector.Storable     as DVS

toByteString :: DVS.Vector Word64 -> BS.ByteString
toByteString v = case DVS.unsafeToForeignPtr (DVS.unsafeCast v) of
  (fptr, vOffset, vLength) -> BS.fromForeignPtr fptr vOffset vLength

rechunkAlignedAt :: Int -> [BS.ByteString] -> [BS.ByteString]
rechunkAlignedAt alignment = go
  where go (bs:bss) = case BS.length bs of
              bsLen -> if bsLen < alignment
                then case alignment - bsLen of
                  bsNeed -> case bss of
                    (cs:css) -> case BS.length cs of
                      csLen | csLen >  bsNeed -> (bs <> BS.take bsNeed cs ):go (BS.drop bsNeed cs:css)
                      csLen | csLen == bsNeed -> (bs <> cs                ):go                    css
                      _     | otherwise       ->                            go ((bs <> cs)       :css)
                    [] -> [bs]
                else case (bsLen `div` alignment) * alignment of
                  bsCroppedLen -> if bsCroppedLen == bsLen
                    then bs:go bss
                    else BS.take bsCroppedLen bs:go (BS.drop bsCroppedLen bs:bss)
        go [] = []

rechunkPaddedAlignedAt :: Int -> [BS.ByteString] -> [BS.ByteString]
rechunkPaddedAlignedAt alignment = go
  where go (bs:bss) = case BS.length bs of
              bsLen -> if bsLen < alignment
                then case alignment - bsLen of
                  bsNeed -> case bss of
                    (cs:css) -> case BS.length cs of
                      csLen | csLen >  bsNeed -> (bs <> BS.take bsNeed cs ):go (BS.drop bsNeed cs:css)
                      csLen | csLen == bsNeed -> (bs <> cs                ):go                    css
                      _     | otherwise       ->                            go ((bs <> cs)       :css)
                    [] -> [bs <> BS.replicate bsNeed 0]
                else case (bsLen `div` alignment) * alignment of
                  bsCroppedLen -> if bsCroppedLen == bsLen
                    then bs:go bss
                    else BS.take bsCroppedLen bs:go (BS.drop bsCroppedLen bs:bss)
        go [] = []

cmpeq8s :: Word8 -> BS.ByteString -> DVS.Vector Word64
cmpeq8s w8 bs = if disalignment == 0
  then DVS.constructN (DVS.length v `div` 8) go
  else error $ "Unaligned byte string: " <> show disalignment
  where bsLength      = BS.length bs
        w64sLen       = bsLength `div` 64
        disalignment  = bsLength - w64sLen * 64
        v             = asVector64 bs
        w64           = fromIntegral w8 * 0x0101010101010101 :: Word64
        go :: DVS.Vector Word64 -> Word64
        go u = comp w
          where ui = fromIntegral $ DVS.length u
                w0 = testWord8s ((u !!! (ui + 0)) .^. w64)
                w1 = testWord8s ((u !!! (ui + 1)) .^. w64)
                w2 = testWord8s ((u !!! (ui + 2)) .^. w64)
                w3 = testWord8s ((u !!! (ui + 3)) .^. w64)
                w4 = testWord8s ((u !!! (ui + 4)) .^. w64)
                w5 = testWord8s ((u !!! (ui + 5)) .^. w64)
                w6 = testWord8s ((u !!! (ui + 6)) .^. w64)
                w7 = testWord8s ((u !!! (ui + 7)) .^. w64)
                w   = (w7 .<. 56) .|.
                      (w6 .<. 48) .|.
                      (w5 .<. 40) .|.
                      (w4 .<. 32) .|.
                      (w3 .<. 24) .|.
                      (w2 .<. 16) .|.
                      (w1 .<.  8) .|.
                       w0
