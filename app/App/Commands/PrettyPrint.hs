{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.PrettyPrint
  ( cmdPrettyPrint
  ) where

import App.Commands.Options.Type
import Control.Lens
import Control.Monad
import Data.Char                 (chr)
import Data.List                 (intersperse, zip)
import Data.Semigroup            ((<>))
import Data.Word
import HaskellWorks.Data.Bits
import Numeric                   (showHex)
import Options.Applicative       hiding (columns)

import qualified App.Commands.Options.Lens           as L
import qualified Data.ByteString                     as BS
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.FromForeignRegion as IO
import qualified System.IO                           as IO

takeRight :: Int -> [a] -> [a]
takeRight n = reverse . take n . reverse

mkByteAndMarker :: BS.ByteString -> DVS.Vector Word64 -> Int -> (Char, Char)
mkByteAndMarker bs v i = (c, b)
  where c = substitute $ chr $ fromIntegral $ BS.index bs i
        b = if DVS.unsafeIndex v (i `div` 64) .&. (1 .<. (fromIntegral i `mod` 64)) /= 0
          then '1'
          else '0'
        substitute :: Char -> Char
        substitute '\n' = '.'
        substitute d    = d

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n
  build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
  build g = g (:) []

runPrettyPrint :: PrettyPrintOptions -> IO ()
runPrettyPrint opts = do
  let indexFilePath = opts ^. L.indexFilePath
  let dataFilePath  = opts ^. L.dataFilePath

  !(bs :: BS.ByteString   ) <- IO.mmapFromForeignRegion dataFilePath
  !(v :: DVS.Vector Word64) <- IO.mmapFromForeignRegion indexFilePath

  let byteAndMarkers = mkByteAndMarker bs v <$> [0..BS.length bs - 1]

  forM_ (zip (chunksOf 64 byteAndMarkers) [0..]) $ \(chunk, i :: Int) -> do
    let (bytes, bits) = unzip chunk

    IO.putStr $ "0x" ++ takeRight 10 ("00000000" ++ showHex (i * 64) "  ")
    IO.putStrLn $ mconcat $ intersperse " " (chunksOf 8 bytes)

    IO.putStr "            "
    IO.putStrLn $ mconcat $ intersperse " " (chunksOf 8 bits)

    IO.putStrLn ""

    return ()


    -- let vi = bi `div` 8
    -- let b = BS.index bs bi
    -- let w = DVS.unsafeIndex v vi
    -- forM_ [0..63] $ \i -> do
    --   if (w .>. i) .&. 1 == 1
    --     then IO.hPutChar IO.stdout '1'
    --     else IO.hPutChar IO.stdout '0'
    --   when (i `mod` 8 == 7) $ do
    --     if (i == 63)
    --       then IO.hPutChar IO.stdout '\n'
    --       else IO.hPutChar IO.stdout ' '

  return ()

optsPrettyPrint :: Parser PrettyPrintOptions
optsPrettyPrint = PrettyPrintOptions
  <$> strOption
        (   long "data-file"
        <>  help "Data file"
        <>  metavar "STRING"
        )
  <*> strOption
        (   long "index-file"
        <>  help "Index file"
        <>  metavar "STRING"
        )

cmdPrettyPrint :: Mod CommandFields (IO ())
cmdPrettyPrint = command "pretty-print"  $ flip info idm $ runPrettyPrint <$> optsPrettyPrint
