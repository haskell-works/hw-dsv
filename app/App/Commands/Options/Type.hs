{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Type where

import GHC.Generics
import GHC.Word     (Word8)

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
  -- , useIndex       :: Bool
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
  { columns        :: [Int]
  , filePath       :: FilePath
  , outputFilePath :: FilePath
  , delimiter      :: Word8
  , outDelimiter   :: Word8
  } deriving (Eq, Show, Generic)
