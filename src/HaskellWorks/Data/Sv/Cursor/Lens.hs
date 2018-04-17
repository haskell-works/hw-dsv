{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module HaskellWorks.Data.Sv.Cursor.Lens where

import Control.Lens
import HaskellWorks.Data.Sv.Cursor.Type

makeFields ''SvCursor
makeFields ''SvMode
