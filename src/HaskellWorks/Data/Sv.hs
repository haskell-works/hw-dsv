module HaskellWorks.Data.Sv
  ( SvCursor(..)
  ) where

data SvCursor t n f = SvCursor
  { svCursorText     :: t
  , svCursorNewlines :: n
  , svCursorFields   :: f
  } deriving (Eq, Show)
