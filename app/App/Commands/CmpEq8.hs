module App.Commands.CmpEq8
  ( cmdCmpEq8
  ) where

import App.Commands.Options.Type
import Control.Lens
import Data.Semigroup            ((<>))
import Options.Applicative

import qualified App.IO                                              as IO
import qualified App.Lens                                            as L
import qualified Data.ByteString                                     as BS
import qualified Data.ByteString.Lazy                                as LBS
import qualified HaskellWorks.Data.Dsv.Internal.Simd.Avx2.ByteString as AVX2BS

runCmpEq8Normal :: CmpEq8Options -> IO ()
runCmpEq8Normal opts = do
  contents <- IO.readInputFile (opts ^. L.source)

  IO.writeOutputFile (opts ^. L.target) ((LBS.fromChunks . fmap BS.copy . LBS.toChunks) contents)

runCmpEq8Simd :: CmpEq8Options -> IO ()
runCmpEq8Simd opts = do
  contents <- IO.readInputFile (opts ^. L.source)

  IO.writeOutputFile (opts ^. L.target) ((LBS.fromChunks . fmap AVX2BS.copy . LBS.toChunks) contents)

runCmpEq8 :: CmpEq8Options -> IO ()
runCmpEq8 opts
  | opts ^. L.simd  = runCmpEq8Simd   opts
  | otherwise       = runCmpEq8Normal opts

optsCmpEq8 :: Parser CmpEq8Options
optsCmpEq8 = CmpEq8Options
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

cmdCmpEq8 :: Mod CommandFields (IO ())
cmdCmpEq8 = command "cmpeq8"  $ flip info idm $ runCmpEq8 <$> optsCmpEq8
