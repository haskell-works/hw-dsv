{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module App.Commands.Options.Lens where

import App.Commands.Options.Type
import Control.Lens

makeFields ''CatOptions
makeFields ''CreateFastIndexOptions
makeFields ''CreateLazyIndexOptions
makeFields ''CreateIndexOptions
makeFields ''GenerateOptions
makeFields ''LazyCountFieldsOptions
makeFields ''PrettyPrintOptions
makeFields ''QueryLazy9Options
makeFields ''QueryOptions
makeFields ''ShowBitsOptions
