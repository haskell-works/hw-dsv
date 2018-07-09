module App.Commands.IndexWord8s
  ( cmdIndexWord8s
  ) where

import App.Commands.Options.Type
import Control.Lens
import Data.Semigroup            ((<>))
import Options.Applicative

import qualified App.IO                                              as IO
import qualified App.Lens                                            as L
import qualified Data.ByteString.Lazy                                as LBS
import qualified HaskellWorks.Data.Dsv.Internal.ByteString           as BS
import qualified HaskellWorks.Data.Dsv.Internal.ByteString.Lazy      as LBS
import qualified HaskellWorks.Data.Dsv.Internal.Simd.Avx2.ByteString as AVX2BS

runIndexWord8sNormal :: IndexWord8sOptions -> IO ()
runIndexWord8sNormal opts = do
  contents <- IO.readInputFile (opts ^. L.source)

  IO.writeOutputFile (opts ^. L.target) ((LBS.fromChunks . BS.rechunkAlignedAt 64 . LBS.toChunks) contents)

runIndexWord8sSimd :: IndexWord8sOptions -> IO ()
runIndexWord8sSimd opts = do
  contents <- IO.readInputFile (opts ^. L.source)

  IO.writeOutputFile (opts ^. L.target) ((LBS.toByteString . fmap (AVX2BS.cmpeq 44) . BS.rechunkPaddedAlignedAt 64 . LBS.toChunks) contents)

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
