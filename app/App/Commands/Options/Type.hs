module App.Commands.Options.Type where

data CreateIndexOptions = CreateIndexOptions
  { _createIndexOptionsFilePath  :: FilePath
  , _createIndexOptionsDelimiter :: Char
  } deriving (Eq, Show)

data QueryStrictOptions = QueryStrictOptions
  { _queryStrictOptionsColumns        :: [Int]
  , _queryStrictOptionsFilePath       :: FilePath
  , _queryStrictOptionsOutputFilePath :: FilePath
  , _queryStrictOptionsDelimiter      :: Char
  , _queryStrictOptionsOutDelimiter   :: Char
  -- , _queryOptionsUseIndex       :: Bool
  } deriving (Eq, Show)

data GenerateOptions = GenerateOptions
  { _generateOptionsFields :: Int
  , _generateOptionsRows   :: Int
  } deriving (Eq, Show)

data CatOptions = CatOptions
  { _catOptionsSource :: FilePath
  , _catOptionsTarget :: FilePath
  } deriving (Eq, Show)

data QueryLazyOptions = QueryLazyOptions
  { _queryLazyOptionsColumns        :: [Int]
  , _queryLazyOptionsFilePath       :: FilePath
  , _queryLazyOptionsOutputFilePath :: FilePath
  , _queryLazyOptionsDelimiter      :: Char
  , _queryLazyOptionsOutDelimiter   :: Char
  } deriving (Eq, Show)
