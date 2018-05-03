module App.Commands.Options.Type where

data CreateIndexOptions = CreateIndexOptions
  { _createIndexOptionsFilePath  :: FilePath
  , _createIndexOptionsDelimiter :: Char
  , _createIndexOptionsClassic   :: Bool
  } deriving (Eq, Show)

data ShowBitsOptions = ShowBitsOptions
  { _showBitsOptionsFilePath :: FilePath
  , _showBitsOptionsPretty   :: Bool
  } deriving (Eq, Show)

data PrettyPrintOptions = PrettyPrintOptions
  { _prettyPrintOptionsDataFilePath  :: FilePath
  , _prettyPrintOptionsIndexFilePath :: FilePath
  } deriving (Eq, Show)

data QueryOptions = QueryOptions
  { _queryOptionsCreateIndex      :: Bool
  , _queryOptionsColumns          :: [Int]
  , _queryOptionsFilePath         :: FilePath
  , _queryOptionsOutputFilePath   :: FilePath
  , _queryOptionsDelimiter        :: Char
  , _queryOptionsOutputBufferSize :: Maybe Int
  } deriving (Eq, Show)
