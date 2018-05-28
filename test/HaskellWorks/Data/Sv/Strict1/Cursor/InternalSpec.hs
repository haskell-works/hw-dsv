{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Sv.Strict1.Cursor.InternalSpec (spec) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString                           (ByteString)
import Data.Char
import Data.List                                 (isSuffixOf)
import Data.Maybe                                (fromJust)
import Data.Text                                 (Text)
import Data.Word
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.FromByteString
import HaskellWorks.Data.Sv.Internal.Broadword
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                              as BS
import qualified Data.Text                                    as T
import qualified Data.Text.Encoding                           as T
import qualified Data.Vector.Storable                         as DVS
import qualified HaskellWorks.Data.FromForeignRegion          as IO
import qualified HaskellWorks.Data.Sv.Gen                     as G
import qualified HaskellWorks.Data.Sv.Internal.Char.Word64    as C
import qualified HaskellWorks.Data.Sv.Strict1.Cursor          as SVS
import qualified HaskellWorks.Data.Sv.Strict1.Cursor.Internal as SVS
import qualified Hedgehog.Gen                                 as G
import qualified Hedgehog.Range                               as R
import qualified System.Directory                             as IO

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Sv.Strict1.Cursor.InternalSpec" $ do
  it "Case 1" $ requireTest $ do
    entries <- liftIO $ IO.listDirectory "data/bench"
    let files = ("data/bench/" ++) <$> (".csv" `isSuffixOf`) `filter` entries
    forM_ files $ \file -> do
      v <- liftIO $ IO.mmapFromForeignRegion file
      let !pccccccc = DVS.foldr (\a b -> popCount1 a + b) 0 (SVS.mkDsvRawBitsByWord64s      C.doubleQuote C.newline C.pipe      v)
      let !actual   = DVS.foldr (\a b -> popCount1 a + b) 0 (SVS.mkIbVector                 '|'                                 v)
      let !expected =     foldr (\a b -> popCount1 a + b) 0 (SVS.mkDsvInterestBitsByWord64s C.doubleQuote C.newline C.pipe 0 0  v)
      annotate $ "file    : " <> file
      annotate $ "pccccccc: " <> show pccccccc
      actual === expected
  it "Case 2" $ requireTest $ do
    bs :: ByteString <- forAll $ T.encodeUtf8 . T.pack <$> G.string (R.linear 0 128) (G.element " \"|\n")
    v <- forAll $ pure $ fromByteString bs
    let !actual   = DVS.foldr (\a b -> popCount1 a + b) 0 (SVS.mkIbVector                 '|'                                 v)
    let !expected =     foldr (\a b -> popCount1 a + b) 0 (SVS.mkDsvInterestBitsByWord64s C.doubleQuote C.newline C.pipe 0 0  v)
    actual === expected
  it "Case 3" $ requireTest $ do
    bs :: ByteString <- forAll $ T.encodeUtf8 . T.pack <$> G.string (R.linear 0 128) (G.element " \"|\n")
    v <- forAll $ pure $ fromByteString bs
    let !actual   = DVS.foldr (\a b -> popCount1 a + b) 0 (SVS.mkIbVector                 '|'                                 v)
    let !expected =     foldr (\a b -> popCount1 a + b) 0 (SVS.mkDsvInterestBitsByWord64s C.doubleQuote C.newline C.pipe 0 0  v)
    actual === expected
  it "Case 4" $ requireTest $ do
    bs :: ByteString <- forAll $ T.encodeUtf8 . T.pack <$> G.string (R.linear 0 10000) (G.element " \"")
    v <- forAll $ pure $ fromByteString bs
    numQuotes <- forAll $ pure $ BS.length $ BS.filter (== fromIntegral (ord '"')) bs
    u <- forAll $ pure $ SVS.mkDsvRawBitsByWord64s C.doubleQuote C.newline C.pipe v
    let !pc = DVS.foldr (\a b -> popCount1 a + b) 0 u
    pc === fromIntegral numQuotes
  it "Case 5" $ requireTest $ do
    bs :: ByteString <- forAll $ T.encodeUtf8 . T.pack <$> G.string (R.linear 0 10000) (G.element " \"")
    v <- forAll $ pure $ fromByteString bs
    numQuotes <- forAll $ pure $ BS.length $ BS.filter (== fromIntegral (ord '"')) bs
    u <- forAll $ pure $ SVS.mkDsvRawBitsByWord64s C.doubleQuote C.newline C.pipe v
    let !expected = DVS.fromList $ SVS.mkDsvInterestBitsByWord64s C.doubleQuote C.newline C.pipe 0 0 v
    let !pc = DVS.foldr (\a b -> popCount1 a + b) 0 u
    u === expected
  it "Case 6" $ requireTest $ do
    entries <- liftIO $ IO.listDirectory "data/bench"
    let files = ("data/bench/" ++) <$> (".csv" `isSuffixOf`) `filter` entries
    forM_ files $ \file -> do
      v <- liftIO $ IO.mmapFromForeignRegion file
      let !actual   = SVS.mkIbVector '|' v
      let !expected = DVS.fromList $ SVS.mkDsvInterestBitsByWord64s C.doubleQuote C.newline C.pipe 0 0 v
      annotate $ "file    : " <> file
      actual === expected


