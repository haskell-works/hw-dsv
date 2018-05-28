{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}

module HaskellWorks.Data.Sv.Strict.Cursor.Type
  ( SvCursor(..)
  ) where

import Data.Word
import HaskellWorks.Data.Container

data SvCursor t s = SvCursor
  { svCursorDelimiter :: Elem t
  , svCursorText      :: !t
  , svCursorMarkers   :: !s
  , svCursorNewlines  :: !s
  , svCursorPosition  :: !Word64
  }

deriving instance (Eq   (Elem t), Eq   t, Eq   s) => Eq   (SvCursor t s)
deriving instance (Show (Elem t), Show t, Show s) => Show (SvCursor t s)
