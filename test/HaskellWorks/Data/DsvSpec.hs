{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.DsvSpec (spec) where

import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Dsv.Internal.Char (comma)
import HaskellWorks.Data.FromByteString
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.Dsv.Strict.Cursor.Internal.Reference as SVS

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

spec :: Spec
spec = describe "HaskellWorks.Data.DsvSpec" $ do
  it "Parsing Basic DSV" $ requireTest $ do
    let bs =  "12345678,12345678,123456,abcdefghijklmnopqrstuvwxyz\n\
              \12345678,12345678,123456,abcdefghijklmnopqrstuvwxyz\n\
              \12345678,12345678,123456,abcdefghijklmnopqrstuvwxyz\n\
              \12345678,12345678,123456,abcdefghijklmnopqrstuvwxyz"

    let v = fromByteString bs
    let actual = SVS.mkIbVector comma v

    bitShow actual ===  "00000000 10000000 01000000 10000000 00000000 00000000 00010000 00001000 \
                        \00000100 00001000 00000000 00000000 00000001 00000000 10000000 01000000 \
                        \10000000 00000000 00000000 00010000 00001000 00000100 00001000 00000000 \
                        \00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
  it "Parsing Quoted DSV" $ requireTest $ do
    let bs =  "12345678,12345678,123456,\"bcdefghijklmnopqrstuvwxy\"\n\
              \12345678,12345678,123456,\"bcdefghijklmnopqrstuvwxy\"\n\
              \12345678,12345678,123456,\"bcdefghijklmnopqrstuvwxy\"\n\
              \12345678,12345678,123456,\"bcdefghijklmnopqrstuvwxy\""

    let v = fromByteString bs
    let !actual = SVS.mkIbVector comma v

    bitShow actual ===  "00000000 10000000 01000000 10000000 00000000 00000000 00010000 00001000 \
                        \00000100 00001000 00000000 00000000 00000001 00000000 10000000 01000000 \
                        \10000000 00000000 00000000 00010000 00001000 00000100 00001000 00000000 \
                        \00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
  it "Parsing Quoted DSV" $ requireTest $ do
    let bs =  "12345678,12345678,123456,\"bcdefghijklm,opqrstuvwxy\"\n\
              \12345678,12345678,123456,\"bcdefghijklm,opqrstuvwxy\"\n\
              \12345678,12345678,123456,\"bcdefghijklm,opqrstuvwxy\"\n\
              \12345678,12345678,123456,\"bcdefghijklm,opqrstuvwxy\""

    let v = fromByteString bs
    let !actual = SVS.mkIbVector comma v

    bitShow actual ===  "00000000 10000000 01000000 10000000 00000000 00000000 00010000 00001000 \
                        \00000100 00001000 00000000 00000000 00000001 00000000 10000000 01000000 \
                        \10000000 00000000 00000000 00010000 00001000 00000100 00001000 00000000 \
                        \00000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"

