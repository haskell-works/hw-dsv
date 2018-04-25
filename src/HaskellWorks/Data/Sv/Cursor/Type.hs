{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}

module HaskellWorks.Data.Sv.Cursor.Type
  ( SvCursor(..)
  , SvMode(..)
  ) where

import Data.Word
import HaskellWorks.Data.Container
import HaskellWorks.Data.Positioning

data SvCursor t s = SvCursor
  { svCursorDelimiter    :: Elem t
  , svCursorText         :: !t
  , svCursorInterestBits :: !s
  , svCursorPosition     :: !Word64
  -- TODO See if there is a faster way to compute popCount field
  , svCursorPopCount     :: !Count
  }

deriving instance (Eq   (Elem t), Eq   t, Eq   s) => Eq   (SvCursor t s)
deriving instance (Show (Elem t), Show t, Show s) => Show (SvCursor t s)

data SvMode = SvUnquoted | SvQuoted deriving (Eq, Show)
