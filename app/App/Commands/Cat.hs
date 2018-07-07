module App.Commands.Cat
  ( cmdCat
  ) where

import App.Commands.Options.Type
import Control.Lens
import Data.Semigroup            ((<>))
import Options.Applicative

import qualified App.IO                                                as IO
import qualified App.Lens                                              as L
import qualified Data.ByteString                                       as BS
import qualified Data.ByteString.Lazy                                  as LBS
import qualified HaskellWorks.Data.Dsv.Internal.Simd.Avx512.ByteString as AVX512BS

runCatNormal :: CatOptions -> IO ()
runCatNormal opts = do
  contents <- IO.readInputFile (opts ^. L.source)

  IO.writeOutputFile (opts ^. L.target) ((LBS.fromChunks . fmap BS.copy . LBS.toChunks) contents)

runCatSimd :: CatOptions -> IO ()
runCatSimd opts = do
  contents <- IO.readInputFile (opts ^. L.source)

  IO.writeOutputFile (opts ^. L.target) ((LBS.fromChunks . fmap AVX512BS.copy . LBS.toChunks) contents)

runCat :: CatOptions -> IO ()
runCat opts
  | opts ^. L.simd  = runCatSimd   opts
  | otherwise       = runCatNormal opts

optsCat :: Parser CatOptions
optsCat = CatOptions
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

cmdCat :: Mod CommandFields (IO ())
cmdCat = command "cat"  $ flip info idm $ runCat <$> optsCat
