{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module HaskellWorks.Data.Sv.Lazy.Cursor.Lens where

import Control.Lens
import HaskellWorks.Data.Sv.Lazy.Cursor.Type

makeFields ''SvCursor
