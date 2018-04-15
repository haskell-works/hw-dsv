module HaskellWorks.Data.Sv.Cursor.Type
  ( SvCursor(..)
  , SvMode(..)
  ) where

import Data.Word

data SvCursor t s = SvCursor
  { svCursorText         :: !t
  , svCursorInterestBits :: !s
  , svCursorPosition     :: !Word64
  } deriving (Eq, Show)

data SvMode = SvUnquoted | SvQuoted deriving (Eq, Show)
