module HaskellWorks.Data.Dsv.Internal.ByteString
  ( toByteString
  , rechunkAlignedAt
  , rechunkPaddedAlignedAt
  ) where

import Data.Monoid ((<>))
import Data.Word

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
