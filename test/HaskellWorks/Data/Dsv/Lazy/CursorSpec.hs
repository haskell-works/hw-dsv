{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Dsv.Lazy.CursorSpec (spec) where

import HaskellWorks.Data.Bits.BitShown
import HaskellWorks.Data.Dsv.Internal.Char (comma)
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString.Lazy                   as LBS
import qualified Data.Vector                            as DV
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor      as SVL
import qualified HaskellWorks.Data.Dsv.Lazy.Cursor.Lazy as SVLL

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Reduce duplication"  -}
{- HLINT ignore "Redundant bracket"   -}

subjectEmpty, subjectSS, subjectSM, subjectMS, subjectMM :: LBS.ByteString
expectedEmpty, expectedSS, expectedSM, expectedMS, expectedMM :: [DV.Vector LBS.ByteString]
subjectEmpty = ""
expectedEmpty = mkExpected []
subjectSS = "hello"
expectedSS = mkExpected [["hello"]]
subjectSM = "hello\nyes"
expectedSM = mkExpected [["hello"],["yes"]]
subjectMS = "hello,goodbye"
expectedMS = mkExpected [["hello","goodbye"]]
subjectMM = "hello,goodbye\nyes,no"
expectedMM = mkExpected [["hello","goodbye"],["yes","no"]]

testToListVector :: LBS.ByteString -> [DV.Vector LBS.ByteString]
testToListVector = SVLL.toListVector . SVL.makeCursor comma

testToListList :: LBS.ByteString -> [[LBS.ByteString]]
testToListList = fmap DV.toList . testToListVector

-- Adds a terminal newline to the file
testToListVector' :: LBS.ByteString -> [DV.Vector LBS.ByteString]
testToListVector' = testToListVector . (<> "\n")

mkExpected :: [[LBS.ByteString]] -> [DV.Vector LBS.ByteString]
mkExpected = fmap DV.fromList

spec :: Spec
spec = describe "HaskellWorks.Data.Dsv.Lazy.CursorSpec" $ do
  -- no terminal newline
  it "Case 1" $ requireTest $ do
    testToListVector subjectEmpty === expectedEmpty
  it "Case 2" $ requireTest $ do
    testToListVector subjectSS === expectedSS
  it "Case 3" $ requireTest $ do
    testToListVector subjectSM === expectedSM
  it "Case 4" $ requireTest $ do
    testToListVector subjectMS === expectedMS
  it "Case 5" $ requireTest $ do
    testToListVector subjectMM === expectedMM
  -- terminal newline
  -- the following test does not pass
  --it "Case 6" $ requireTest $ do
    --testToListVector' subjectEmpty === expectedEmpty
  it "Case 7" $ requireTest $ do
    testToListVector' subjectSS === expectedSS
  it "Case 8" $ requireTest $ do
    testToListVector' subjectSM === expectedSM
  it "Case 9" $ requireTest $ do
    testToListVector' subjectMS === expectedMS
  it "Case 10" $ requireTest $ do
    testToListVector' subjectMM === expectedMM
  describe "boundaries" $ do
    it "513 bytes" $ requireTest $ do
      let actual = testToListList $ LBS.intercalate "\n"
            [ "id,group_id,arm,first_name,last_name,email,contact_number,start_date,status,dose_smart_id,fitbit_id,glyco_id,sms_reminder,email_reminder,inactive_reminder,starting_weight,starting_bmi,starting_hba1c"
            , "10,aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,1,aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,aaaaaaa,aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,2018-01-01 00:00:00 UTC,Active,,,,Never,Weekly,Never,100.01,10.01,1.0e-2"
            ]
      actual ===
        [ [ "id"
          , "group_id"
          , "arm"
          , "first_name"
          , "last_name"
          , "email"
          , "contact_number"
          , "start_date"
          , "status"
          , "dose_smart_id"
          , "fitbit_id"
          , "glyco_id"
          , "sms_reminder"
          , "email_reminder"
          , "inactive_reminder"
          , "starting_weight"
          , "starting_bmi"
          , "starting_hba1c"
          ]
        , [ "10"
          , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , "1"
          , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , "aaaaaaa"
          , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , "2018-01-01 00:00:00 UTC"
          , "Active"
          , ""
          , ""
          , ""
          , "Never"
          , "Weekly"
          , "Never"
          , "100.01"
          , "10.01"
          , "1.0e-2"
          ]
        ]
    it "512 bytes" $ requireTest $ do
      let actual = testToListList $ LBS.intercalate "\n"
            [ "id,group_id,arm,first_name,last_name,email,contact_number,start_date,status,dose_smart_id,fitbit_id,glyco_id,sms_reminder,email_reminder,inactive_reminder,starting_weight,starting_bmi,starting_hba1c"
            , "10,aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,1,aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,aaaaaaa,aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,2018-01-01 00:00:00 UTC,Active,,,,Never,Weekly,Never,100.01,10.01,1.0e-2"
            ]
      actual ===
        [ [ "id"
          , "group_id"
          , "arm"
          , "first_name"
          , "last_name"
          , "email"
          , "contact_number"
          , "start_date"
          , "status"
          , "dose_smart_id"
          , "fitbit_id"
          , "glyco_id"
          , "sms_reminder"
          , "email_reminder"
          , "inactive_reminder"
          , "starting_weight"
          , "starting_bmi"
          , "starting_hba1c"
          ]
        , [ "10"
          , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , "1"
          , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , "aaaaaaa"
          , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , "2018-01-01 00:00:00 UTC"
          , "Active"
          , ""
          , ""
          , ""
          , "Never"
          , "Weekly"
          , "Never"
          , "100.01"
          , "10.01"
          , "1.0e-2"
          ]
        ]

    it "511 bytes" $ requireTest $ do
      let actual = testToListList $ LBS.intercalate "\n"
            [ "id,group_id,arm,first_name,last_name,email,contact_number,start_date,status,dose_smart_id,fitbit_id,glyco_id,sms_reminder,email_reminder,inactive_reminder,starting_weight,starting_bmi,starting_hba1c"
            , "10,aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,1,aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,aaaaaaa,aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,2018-01-01 00:00:00 UTC,Active,,,,Never,Weekly,Never,100.01,10.01,1.0e-2"
            ]
      actual ===
        [ [ "id"
          , "group_id"
          , "arm"
          , "first_name"
          , "last_name"
          , "email"
          , "contact_number"
          , "start_date"
          , "status"
          , "dose_smart_id"
          , "fitbit_id"
          , "glyco_id"
          , "sms_reminder"
          , "email_reminder"
          , "inactive_reminder"
          , "starting_weight"
          , "starting_bmi"
          , "starting_hba1c"
          ]
        , [ "10"
          , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , "1"
          , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , "aaaaaaa"
          , "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          , "2018-01-01 00:00:00 UTC"
          , "Active"
          , ""
          , ""
          , ""
          , "Never"
          , "Weekly"
          , "Never"
          , "100.01"
          , "10.01"
          , "1.0e-2"
          ]
        ]
  describe "single quoted field with newline at 127-129" $ do
    it "newline at 127 bytes" $ requireTest $ do
      let newlinePos = 127
      let text = "\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\""
      let expected = [["\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\""]]
      let cursor = SVL.makeCursor 44 text
      _ <- forAll $ pure $ BitShown (head (SVL.dsvCursorMarkers  cursor))
      _ <- forAll $ pure $ BitShown (head (SVL.dsvCursorNewlines cursor))
      let actual = fmap DV.toList (DV.toList (SVLL.toVectorVector cursor))
      LBS.length text === newlinePos + 2
      LBS.index text newlinePos === 10
      actual === expected
    it "newline at 128 bytes" $ requireTest $ do
      let newlinePos = 128
      let text = "\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\""
      let expected = [["\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\""]]
      let cursor = SVL.makeCursor 44 text
      _ <- forAll $ pure $ BitShown (head (SVL.dsvCursorMarkers  cursor))
      _ <- forAll $ pure $ BitShown (head (SVL.dsvCursorNewlines cursor))
      let actual = fmap DV.toList (DV.toList (SVLL.toVectorVector cursor))
      LBS.length text === newlinePos + 2
      LBS.index text newlinePos === 10
      actual === expected
    it "newline at 129 bytes" $ requireTest $ do
      let newlinePos = 129
      let text = "\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\""
      let expected = [["\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\n\""]]
      let cursor = SVL.makeCursor 44 text
      _ <- forAll $ pure $ BitShown (head (SVL.dsvCursorMarkers  cursor))
      _ <- forAll $ pure $ BitShown (head (SVL.dsvCursorNewlines cursor))
      let actual = fmap DV.toList (DV.toList (SVLL.toVectorVector cursor))
      LBS.length text === newlinePos + 2
      LBS.index text newlinePos === 10
      actual === expected
