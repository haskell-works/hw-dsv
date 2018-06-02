{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}

module HaskellWorks.Data.Dsv.Strict.Cursor.Type
  ( DsvCursor(..)
  ) where

import Data.Word
import HaskellWorks.Data.Container

data DsvCursor t s = DsvCursor
  { dsvCursorDelimiter :: Elem t
  , dsvCursorText      :: !t
  , dsvCursorMarkers   :: !s
  , dsvCursorNewlines  :: !s
  , dsvCursorPosition  :: !Word64
  }

deriving instance (Eq   (Elem t), Eq   t, Eq   s) => Eq   (DsvCursor t s)
deriving instance (Show (Elem t), Show t, Show s) => Show (DsvCursor t s)
