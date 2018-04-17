{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}

module HaskellWorks.Data.Sv.Cursor.Type
  ( SvCursor(..)
  , SvMode(..)
  ) where

import Data.Word
import HaskellWorks.Data.Container

data SvCursor t s = SvCursor
  { svCursorDelimiter    :: Elem t
  , svCursorText         :: !t
  , svCursorInterestBits :: !s
  , svCursorPosition     :: !Word64
  }

deriving instance (Eq   (Elem t), Eq   t, Eq   s) => Eq   (SvCursor t s)
deriving instance (Show (Elem t), Show t, Show s) => Show (SvCursor t s)

data SvMode = SvUnquoted | SvQuoted deriving (Eq, Show)
