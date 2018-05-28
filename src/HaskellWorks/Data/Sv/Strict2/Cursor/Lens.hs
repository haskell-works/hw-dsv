{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module HaskellWorks.Data.Sv.Strict2.Cursor.Lens where

import Control.Lens
import HaskellWorks.Data.Sv.Strict2.Cursor.Type

makeFields ''SvCursor
