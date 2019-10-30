{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module App.Data.ColumnDesc
  ( ColumnDesc(..)
  , realiseColumnDescLazy
  , realiseColumnDescStrict
  , columnDescToTuple
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Text
import GHC.Generics

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS

data ColumnDesc a = ColumnDesc
    { number     :: Int
    , conversion :: a
    } deriving (Eq, Show, Generic, Functor)

realiseColumnDescLazy :: ColumnDesc Text -> ColumnDesc (LBS.ByteString -> LBS.ByteString)
realiseColumnDescLazy cd = fmap realise cd
  where realise :: Text -> LBS.ByteString -> LBS.ByteString
        realise t = case t of
          "id"             -> id
          "ipv4-to-word32" -> const "xxx"
          _                -> const "BAD"

realiseColumnDescStrict :: ColumnDesc Text -> ColumnDesc (BS.ByteString -> BS.ByteString)
realiseColumnDescStrict cd = fmap (\f -> LBS.toStrict . f . LBS.fromStrict) (realiseColumnDescLazy cd)

columnDescToTuple :: ColumnDesc a -> (Int, a)
columnDescToTuple cd = (cd ^. the @1, cd ^. the @2)
