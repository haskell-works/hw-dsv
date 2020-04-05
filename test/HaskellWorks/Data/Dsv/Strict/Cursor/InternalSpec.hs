{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Dsv.Strict.Cursor.InternalSpec (spec) where

import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString                           (ByteString)
import Data.Char
import Data.List                                 (isSuffixOf)
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Dsv.Internal.Char       (pipe)
import HaskellWorks.Data.FromByteString
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString                                        as BS
import qualified Data.Text                                              as T
import qualified Data.Text.Encoding                                     as T
import qualified Data.Vector.Storable                                   as DVS
import qualified HaskellWorks.Data.Dsv.Strict.Cursor.Internal           as SVS
import qualified HaskellWorks.Data.Dsv.Strict.Cursor.Internal.Reference as SVS
import qualified HaskellWorks.Data.FromForeignRegion                    as IO
import qualified Hedgehog.Gen                                           as G
import qualified Hedgehog.Range                                         as R
import qualified System.Directory                                       as IO

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Dsv.Strict.Cursor.InternalSpec" $ do
  it "Case 1" $ requireTest $ do
    entries <- liftIO $ IO.listDirectory "data/bench"
    let files = ("data/bench/" ++) <$> (".csv" `isSuffixOf`) `filter` entries
    forM_ files $ \file -> do
      v <- liftIO $ IO.mmapFromForeignRegion file
      let !actual   = DVS.foldr (\a b -> popCount1 a + b) 0 (fst $  SVS.makeIndexes pipe v)
      let !expected = DVS.foldr (\a b -> popCount1 a + b) 0 (       SVS.mkIbVector  pipe v)
      actual === expected
  it "Case 2" $ requireTest $ do
    bs :: ByteString <- forAll $ T.encodeUtf8 . T.pack <$> G.string (R.linear 0 128) (G.element " \"|\n")
    v <- forAll $ pure $ fromByteString bs
    let !actual   = DVS.foldr (\a b -> popCount1 a + b) 0 (fst $ SVS.makeIndexes pipe v)
    let !expected = DVS.foldr (\a b -> popCount1 a + b) 0 (      SVS.mkIbVector  pipe v)
    actual === expected
  it "Case 3" $ requireTest $ do
    bs :: ByteString <- forAll $ T.encodeUtf8 . T.pack <$> G.string (R.linear 0 128) (G.element " \"|\n")
    v <- forAll $ pure $ fromByteString bs
    let !actual   = DVS.foldr (\a b -> popCount1 a + b) 0 (fst $ SVS.makeIndexes pipe v)
    let !expected = DVS.foldr (\a b -> popCount1 a + b) 0 (      SVS.mkIbVector  pipe v)
    actual === expected
  it "Case 4" $ requireTest $ do
    bs :: ByteString <- forAll $ T.encodeUtf8 . T.pack <$> G.string (R.linear 0 10000) (G.element " \"")
    v <- forAll $ pure $ fromByteString bs
    numQuotes <- forAll $ pure $ BS.length $ BS.filter (== fromIntegral (ord '"')) bs
    u <- forAll $ pure $ fst $ SVS.makeIndexes pipe v
    let !pc = DVS.foldr (\a b -> popCount1 a + b) 0 u
    pc === fromIntegral numQuotes
  it "Case 5" $ requireTest $ do
    bs :: ByteString <- forAll $ T.encodeUtf8 . T.pack <$> G.string (R.linear 0 10000) (G.element " \"")
    v <- forAll $ pure $ fromByteString bs
    u <- forAll $ pure $ fst $ SVS.makeIndexes pipe v
    let !expected =            SVS.mkIbVector  pipe v
    u === expected
  it "Case 6" $ requireTest $ do
    entries <- liftIO $ IO.listDirectory "data/bench"
    let files = ("data/bench/" ++) <$> (".csv" `isSuffixOf`) `filter` entries
    forM_ files $ \file -> do
      v <- liftIO $ IO.mmapFromForeignRegion file
      let !actual   = fst $ SVS.makeIndexes pipe v
      let !expected =       SVS.mkIbVector  pipe v
      annotate $ "file    : " <> file
      actual === expected
