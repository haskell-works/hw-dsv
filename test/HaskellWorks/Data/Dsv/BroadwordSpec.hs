{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Dsv.BroadwordSpec (spec) where

import Data.Maybe                               (fromJust)
import Data.Word
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Dsv.Internal.Broadword
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

spec :: Spec
spec = describe "HaskellWorks.Data.Dsv.BroadwordSpec" $ do
  it "Case 0" $ requireProperty $ do
    let actual    = toggle64 0 $ fromJust $ bitRead "00100100 00000000 00000000 00000000 00000000 00000000 00000000 00000000" :: Word64
    let expected  =              fromJust $ bitRead "11000111 11111111 11111111 11111111 11111111 11111111 11111111 11111111" :: Word64
    bitShow actual === bitShow expected
  it "Case 1" $ requireProperty $ do
    let actual    = toggle64 0 $ fromJust $ bitRead "00100100 00000100 00000000 00000000 00000000 00000000 00000010 00001000" :: Word64
    let expected  =              fromJust $ bitRead "11000111 11111000 00000000 00000000 00000000 00000000 00000011 11110000" :: Word64
    bitShow actual === bitShow expected
  it "Case 1" $ requireProperty $ do
    let actual    = toggle64 1 $ fromJust $ bitRead "00100100 00000100 00000000 00000000 00000000 00000000 00000010 00001000" :: Word64
    let expected  =              fromJust $ bitRead "00111000 00000111 11111111 11111111 11111111 11111111 11111100 00001111" :: Word64
    bitShow actual === bitShow expected

{-
    normal case
    -----------
    00100100
    11011011

    00010000
    11011011 +

    11000111
    carry case
    -----------
    00100100
    11011011
      |  |
    10000010
    11011011 +
    00111000
-}
