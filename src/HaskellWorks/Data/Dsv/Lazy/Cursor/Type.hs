module HaskellWorks.Data.Dsv.Lazy.Cursor.Type where

import Data.Word

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector.Storable as DVS

data DsvCursor = DsvCursor
  { dsvCursorDelimiter :: Word8
  , dsvCursorText      :: !LBS.ByteString
  , dsvCursorMarkers   :: ![DVS.Vector Word64]
  , dsvCursorNewlines  :: ![DVS.Vector Word64]
  , dsvCursorPosition  :: !Word64
  } deriving (Eq, Show)
