{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Type where

import App.Data.ColumnDesc
import Data.Text           (Text)
import GHC.Generics
import GHC.Word            (Word8)

data CreateIndexOptions = CreateIndexOptions
  { filePath  :: FilePath
  , delimiter :: Word8
  } deriving (Eq, Show, Generic)

data QueryStrictOptions = QueryStrictOptions
  { columns        :: [Int]
  , filePath       :: FilePath
  , outputFilePath :: FilePath
  , delimiter      :: Word8
  , outDelimiter   :: Word8
  , useIndex       :: Bool
  } deriving (Eq, Show, Generic)

data GenerateOptions = GenerateOptions
  { fields :: Int
  , rows   :: Int
  } deriving (Eq, Show, Generic)

data CatOptions = CatOptions
  { source :: FilePath
  , target :: FilePath
  , simd   :: Bool
  } deriving (Eq, Show, Generic)

data IndexWord8sOptions = IndexWord8sOptions
  { source :: FilePath
  , target :: FilePath
  , simd   :: Bool
  } deriving (Eq, Show, Generic)

data QueryLazyOptions = QueryLazyOptions
  { columns        :: [ColumnDesc Text]
  , filePath       :: FilePath
  , outputFilePath :: FilePath
  , delimiter      :: Word8
  , outDelimiter   :: Word8
  , method         :: String
  } deriving (Eq, Show, Generic)

data RangeJoinOptions = RangeJoinOptions
  { input1FilePath    :: FilePath
  , input1Delimiter   :: Word8
  , input1StartColumn :: Int
  , input1StopColumn  :: Int
  , input2FilePath    :: FilePath
  , input2Delimiter   :: Word8
  , input2StartColumn :: Int
  , input2StopColumn  :: Int
  , rangeType         :: Text
  , outputFilePath    :: FilePath
  , outputDelimiter   :: Word8
  } deriving (Eq, Show, Generic)
