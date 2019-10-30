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

import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import qualified HaskellWorks.Data.Network.Ip.Ipv4 as IPv4
import qualified Text.Appar.String                 as TAS

data ColumnDesc a = ColumnDesc
    { number     :: Int
    , conversion :: a
    } deriving (Eq, Show, Generic, Functor)

ipv4ToWord32 :: LBS.ByteString -> LBS.ByteString
ipv4ToWord32 lbs = case TAS.parse IPv4.parseIpAddress (T.unpack (T.decodeUtf8 (LBS.toStrict lbs))) of
  Just ipv4Address -> LBS.fromStrict (T.encodeUtf8 (T.pack (show (ipv4Address ^. the @"word"))))
  Nothing          -> lbs

realiseColumnDescLazy :: ColumnDesc Text -> ColumnDesc (LBS.ByteString -> LBS.ByteString)
realiseColumnDescLazy cd = fmap realise cd
  where realise :: Text -> LBS.ByteString -> LBS.ByteString
        realise t = case t of
          "id"             -> id
          "ipv4-to-word32" -> ipv4ToWord32
          _                -> const "BAD"

realiseColumnDescStrict :: ColumnDesc Text -> ColumnDesc (BS.ByteString -> BS.ByteString)
realiseColumnDescStrict cd = fmap (\f -> LBS.toStrict . f . LBS.fromStrict) (realiseColumnDescLazy cd)

columnDescToTuple :: ColumnDesc a -> (Int, a)
columnDescToTuple cd = (cd ^. the @1, cd ^. the @2)
