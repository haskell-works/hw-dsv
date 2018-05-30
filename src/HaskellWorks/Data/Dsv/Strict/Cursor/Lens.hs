{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module HaskellWorks.Data.Dsv.Strict.Cursor.Lens where

import Control.Lens
import HaskellWorks.Data.Dsv.Strict.Cursor.Type

makeFields ''SvCursor
