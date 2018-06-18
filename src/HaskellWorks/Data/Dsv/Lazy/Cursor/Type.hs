{-# LANGUAGE DeriveGeneric #-}

module HaskellWorks.Data.Dsv.Lazy.Cursor.Type where

import Control.DeepSeq (NFData)
import Data.Word
import GHC.Generics (Generic)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector.Storable as DVS

data DsvCursor = DsvCursor
  { dsvCursorText     :: !LBS.ByteString
  , dsvCursorMarkers  :: ![DVS.Vector Word64]
  , dsvCursorNewlines :: ![DVS.Vector Word64]
  , dsvCursorPosition :: !Word64
  } deriving (Eq, Show, Generic)

instance NFData DsvCursor
