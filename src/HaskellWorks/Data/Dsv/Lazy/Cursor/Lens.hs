{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module HaskellWorks.Data.Dsv.Lazy.Cursor.Lens where

import Control.Lens
import HaskellWorks.Data.Dsv.Lazy.Cursor.Type

makeFields ''SvCursor
