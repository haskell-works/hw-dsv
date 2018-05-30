module HaskellWorks.Data.Dsv.Gen where

import Hedgehog

import qualified Data.ByteString as BS
import qualified Hedgehog.Gen    as G
import qualified Hedgehog.Range  as R

bytestring :: MonadGen m => R.Range Int -> m BS.ByteString
bytestring r = BS.pack <$> G.list r (G.word8 R.constantBounded)
