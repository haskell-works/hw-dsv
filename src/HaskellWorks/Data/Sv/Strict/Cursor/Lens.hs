{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module HaskellWorks.Data.Sv.Strict.Cursor.Lens where

import Control.Lens
import HaskellWorks.Data.Sv.Strict.Cursor.Type

makeFields ''LazyCursor
makeFields ''SvCursor
makeFields ''SvCursor2
makeFields ''SvMode
