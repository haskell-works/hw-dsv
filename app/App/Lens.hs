{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module App.Lens where

import App.Commands.Options.Type
import Control.Lens

makeFields ''CatOptions
makeFields ''IndexWord8sOptions
makeFields ''CreateIndexOptions
makeFields ''GenerateOptions
makeFields ''QueryLazyOptions
makeFields ''QueryStrictOptions
