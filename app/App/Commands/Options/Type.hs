module App.Commands.Options.Type where

import GHC.Word (Word8)

data CreateIndexOptions = CreateIndexOptions
  { _createIndexOptionsFilePath  :: FilePath
  , _createIndexOptionsDelimiter :: Word8
  } deriving (Eq, Show)

data QueryStrictOptions = QueryStrictOptions
  { _queryStrictOptionsColumns        :: [Int]
  , _queryStrictOptionsFilePath       :: FilePath
  , _queryStrictOptionsOutputFilePath :: FilePath
  , _queryStrictOptionsDelimiter      :: Word8
  , _queryStrictOptionsOutDelimiter   :: Word8
  -- , _queryOptionsUseIndex       :: Bool
  } deriving (Eq, Show)

data GenerateOptions = GenerateOptions
  { _generateOptionsFields :: Int
  , _generateOptionsRows   :: Int
  } deriving (Eq, Show)

data CatOptions = CatOptions
  { _catOptionsSource :: FilePath
  , _catOptionsTarget :: FilePath
  , _catOptionsSimd   :: Bool
  } deriving (Eq, Show)

data CmpEq8Options = CmpEq8Options
  { _cmpEq8OptionsSource :: FilePath
  , _cmpEq8OptionsTarget :: FilePath
  , _cmpEq8OptionsSimd   :: Bool
  } deriving (Eq, Show)

data QueryLazyOptions = QueryLazyOptions
  { _queryLazyOptionsColumns        :: [Int]
  , _queryLazyOptionsFilePath       :: FilePath
  , _queryLazyOptionsOutputFilePath :: FilePath
  , _queryLazyOptionsDelimiter      :: Word8
  , _queryLazyOptionsOutDelimiter   :: Word8
  } deriving (Eq, Show)
