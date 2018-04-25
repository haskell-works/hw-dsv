{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module App.Commands.Options.Lens where

import App.Commands.Options.Type
import Control.Lens

makeFields ''CreateIndexOptions
makeFields ''ShowBitsOptions
makeFields ''PrettyPrintOptions
