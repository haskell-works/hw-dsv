{-# LANGUAGE OverloadedStrings #-}

module App.Commands.Options.Parse
  ( nonZeroOneBased
  , columnDesc
  ) where

import Data.Text
import Options.Applicative
import Text.Read           (readEither)

import qualified App.Data.ColumnDesc as Z
import qualified App.Data.List       as L
import qualified Data.Text           as T

nonZeroOneBased :: Mod OptionFields Int -> Parser Int
nonZeroOneBased = option $ eitherReader $ \s -> do
  a <- readEither s
  if a == 0
    then Left "cannot index column 0"
    else Right (a - 1)

columnDesc :: Mod OptionFields (Z.ColumnDesc Text) -> Parser (Z.ColumnDesc Text)
columnDesc = option $ eitherReader $ \s -> do
  case L.splitOn ':' s of
    [iString, f] -> do
      i <- readEither iString
      if i == 0
        then Left "cannot index column 0"
        else Right (Z.ColumnDesc (i - 1) (T.pack f))
    [iString] -> do
      i <- readEither iString
      if i == 0
        then Left "cannot index column 0"
        else Right (Z.ColumnDesc (i - 1) "id")
    _ -> Left $ "invalid column description " <> show s
