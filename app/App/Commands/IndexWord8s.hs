{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.IndexWord8s
  ( cmdIndexWord8s
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.Semigroup                      ((<>))
import HaskellWorks.Data.Vector.AsVector64
import Options.Applicative

import qualified App.Commands.Options.Type               as Z
import qualified App.IO                                  as IO
import qualified Data.ByteString.Lazy                    as LBS
import qualified HaskellWorks.Data.ByteString            as BS
import qualified HaskellWorks.Data.ByteString.Lazy       as LBS
import qualified HaskellWorks.Data.Simd.Comparison.Avx2  as AVX2
import qualified HaskellWorks.Data.Simd.Comparison.Stock as STOCK

runIndexWord8sNormal :: Z.IndexWord8sOptions -> IO ()
runIndexWord8sNormal opts = do
  contents <- IO.readInputFile (opts ^. the @"source")

  IO.writeOutputFile (opts ^. the @"target") $ contents
    & LBS.toLazyByteString
    . fmap (STOCK.cmpEqWord8s 44 . asVector64)
    . BS.resegmentPadded 64
    . LBS.toChunks

runIndexWord8sSimd :: Z.IndexWord8sOptions -> IO ()
runIndexWord8sSimd opts = do
  contents <- IO.readInputFile (opts ^. the @"source")

  IO.writeOutputFile (opts ^. the @"target") $ contents
    & LBS.toLazyByteString
    . fmap (AVX2.cmpEqWord8s 44 . asVector64)
    . BS.resegmentPadded 64
    . LBS.toChunks

runIndexWord8s :: Z.IndexWord8sOptions -> IO ()
runIndexWord8s opts
  | opts ^. the @"simd"  = runIndexWord8sSimd   opts
  | otherwise       = runIndexWord8sNormal opts

optsIndexWord8s :: Parser Z.IndexWord8sOptions
optsIndexWord8s = Z.IndexWord8sOptions
  <$> strOption
        (   long "input"
        <>  help "Input file"
        <>  metavar "FILE"
        <>  showDefault
        <>  value "-"
        )
  <*> strOption
        (   long "output"
        <>  help "Output file"
        <>  metavar "FILE"
        <>  showDefault
        <>  value "-"
        )
  <*> switch
        (   long "simd"
        <>  help "Use simd method"
        )

cmdIndexWord8s :: Mod CommandFields (IO ())
cmdIndexWord8s = command "index-word8s"  $ flip info idm $ runIndexWord8s <$> optsIndexWord8s
