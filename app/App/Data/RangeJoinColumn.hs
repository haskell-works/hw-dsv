{-# LANGUAGE DeriveGeneric #-}

module App.Data.RangeJoinColumn
  ( RangeJoinColumn(..)
  ) where

import GHC.Generics

data RangeJoinColumn = LtColumn Int | RtColumn Int
  deriving (Eq, Show, Generic)
