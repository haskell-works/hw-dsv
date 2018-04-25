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
