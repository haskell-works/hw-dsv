{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}

module HaskellWorks.Data.Sv.Strict.Cursor.Type
  ( SvCursor(..)
  , SvCursor2(..)
  , SvMode(..)
  ) where

import Data.Word
import HaskellWorks.Data.Container
import HaskellWorks.Data.Positioning

data SvCursor t s = SvCursor
  { svCursorDelimiter :: Elem t
  , svCursorText      :: !t
  , svCursorMarkers   :: !s
  , svCursorPosition  :: !Word64
  , svCursorPopCount  :: !Count
  }

deriving instance (Eq   (Elem t), Eq   t, Eq   s) => Eq   (SvCursor t s)
deriving instance (Show (Elem t), Show t, Show s) => Show (SvCursor t s)

data SvCursor2 t s = SvCursor2
  { svCursor2Delimiter   :: Elem t
  , svCursor2Text        :: !t
  , svCursor2IbNewline   :: !s
  , svCursor2IbDelimiter :: !s
  , svCursor2Position    :: !Word64
  }

deriving instance (Eq   (Elem t), Eq   t, Eq   s) => Eq   (SvCursor2 t s)
deriving instance (Show (Elem t), Show t, Show s) => Show (SvCursor2 t s)

data SvMode = SvUnquoted | SvQuoted deriving (Eq, Show)
