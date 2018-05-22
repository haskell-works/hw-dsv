module HaskellWorks.Data.Sv.Lazy.Cursor.Type where

import Data.Word

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector.Storable as DVS

data SvCursor = SvCursor
  { svCursorDelimiter :: Word8
  , svCursorText      :: !LBS.ByteString
  , svCursorMarkers   :: ![DVS.Vector Word64]
  , svCursorNewlines  :: ![DVS.Vector Word64]
  , svCursorPosition  :: !Word64
  } deriving (Eq, Show)
