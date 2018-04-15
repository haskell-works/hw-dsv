{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module HaskellWorks.Data.Sv.Cursor.Lens where

import Control.Lens
import HaskellWorks.Data.Sv.Cursor.Type

makeFields ''SvCursor
makeFields ''SvMode
