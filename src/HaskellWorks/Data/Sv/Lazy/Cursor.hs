module HaskellWorks.Data.Sv.Lazy.Cursor where

import Data.Word

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector.Storable as DVS

data SvCursor9 = SvCursor9
  { svCursor9Delimiter    :: Word8
  , svCursor9Text         :: !LBS.ByteString
  , svCursor9InterestBits :: ![DVS.Vector Word64]
  , svCursor9Newlines     :: ![DVS.Vector Word64]
  , svCursor9Position     :: !Word64
  } deriving (Eq, Show)
