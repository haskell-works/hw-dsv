{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module HaskellWorks.Data.Sv.Strict1.Cursor.Lens where

import Control.Lens
import HaskellWorks.Data.Sv.Strict1.Cursor.Type

makeFields ''SvCursor
