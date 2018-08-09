module App.Commands.IndexWord8s
  ( cmdIndexWord8s
  ) where

import App.Commands.Options.Type
import Control.Lens
import Data.Semigroup                      ((<>))
import HaskellWorks.Data.Vector.AsVector64
import Options.Applicative

import qualified App.IO                                          as IO
import qualified App.Lens                                        as L
import qualified Data.ByteString.Lazy                            as LBS
import qualified HaskellWorks.Data.Dsv.Internal.ByteString       as BS
import qualified HaskellWorks.Data.Simd.Comparison.Avx2          as AVX2
import qualified HaskellWorks.Data.Simd.Comparison.Stock         as STOCK
import qualified HaskellWorks.Data.Simd.Internal.ByteString.Lazy as LBS

runIndexWord8sNormal :: IndexWord8sOptions -> IO ()
runIndexWord8sNormal opts = do
  contents <- IO.readInputFile (opts ^. L.source)

  IO.writeOutputFile (opts ^. L.target) $ contents
    & LBS.toByteString
    . fmap (STOCK.cmpEqWord8s 44 . asVector64)
    . BS.rechunkPaddedAlignedAt 64
    . LBS.toChunks

runIndexWord8sSimd :: IndexWord8sOptions -> IO ()
runIndexWord8sSimd opts = do
  contents <- IO.readInputFile (opts ^. L.source)

  IO.writeOutputFile (opts ^. L.target) $ contents
    & LBS.toByteString
    . fmap (AVX2.cmpEqWord8s 44 . asVector64)
    . BS.rechunkPaddedAlignedAt 64
    . LBS.toChunks

runIndexWord8s :: IndexWord8sOptions -> IO ()
runIndexWord8s opts
  | opts ^. L.simd  = runIndexWord8sSimd   opts
  | otherwise       = runIndexWord8sNormal opts

optsIndexWord8s :: Parser IndexWord8sOptions
optsIndexWord8s = IndexWord8sOptions
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
