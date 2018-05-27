{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module HaskellWorks.Data.Sv.Strict.Load
  ( SVS.SvCursor(..)
  , mmapCursor
  , countFields
  ) where

import Data.Char                                 (ord)
import Data.Word
import HaskellWorks.Data.Product
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.RankSelect.CsPoppy

import qualified Data.ByteString                             as BS
import qualified Data.Vector.Storable                        as DVS
import qualified HaskellWorks.Data.FromForeignRegion         as IO
import qualified HaskellWorks.Data.Sv.Strict.Cursor          as SVS
import qualified HaskellWorks.Data.Sv.Strict.Cursor.Internal as SVS

{-# ANN module ("HLint: ignore Redundant guard"        :: String) #-}

mmapCursor :: Char -> Bool -> FilePath -> IO (SVS.SvCursor BS.ByteString CsPoppy)
mmapCursor delimiter createIndex filePath = do
  (!bs) :*: (!v) <- IO.mmapFromForeignRegion filePath
  let !_ = v :: DVS.Vector Word64
  !ibIndex <- makeCsPoppy <$> if createIndex
    then return $ SVS.mkIbVector delimiter v
    else IO.mmapFromForeignRegion (filePath ++ ".ib")
  return SVS.SvCursor
    { SVS.svCursorDelimiter = fromIntegral (ord delimiter)
    , SVS.svCursorText      = bs
    , SVS.svCursorMarkers   = ibIndex
    , SVS.svCursorPosition  = 0
    }

countFields :: forall s. (Rank1 s, Select1 s) => SVS.SvCursor BS.ByteString s -> Int
countFields = go 0
  where go n d = case SVS.nextInterestingBit d of
          e -> case SVS.nextPosition e of
            Just f  -> go (n + 1) f
            Nothing -> n
