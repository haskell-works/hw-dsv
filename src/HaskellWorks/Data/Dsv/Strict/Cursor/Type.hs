{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module HaskellWorks.Data.Dsv.Strict.Cursor.Type
  ( DsvCursor(..)
  ) where

import Control.DeepSeq
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Container

data DsvCursor t s = DsvCursor
  { dsvCursorDelimiter :: Elem t
  , dsvCursorText      :: !t
  , dsvCursorMarkers   :: !s
  , dsvCursorNewlines  :: !s
  , dsvCursorPosition  :: !Word64
  }
  deriving Generic

deriving instance (Eq   (Elem t), Eq   t, Eq   s) => Eq   (DsvCursor t s)
deriving instance (Show (Elem t), Show t, Show s) => Show (DsvCursor t s)

instance (NFData t, NFData (Elem t), NFData s) => NFData (DsvCursor t s)
