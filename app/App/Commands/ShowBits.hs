{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App.Commands.ShowBits
  ( cmdShowBits
  ) where

import App.Commands.Options.Type
import Control.Lens
import Control.Monad
import Data.Word
import HaskellWorks.Data.Bits
import Options.Applicative       hiding (columns)

import qualified App.Commands.Options.Lens           as L
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.FromForeignRegion as IO
import qualified System.IO                           as IO

runShowBits :: ShowBitsOptions -> IO ()
runShowBits opts = do
  let filePath  = opts ^. L.filePath

  !(v :: DVS.Vector Word64) <- IO.mmapFromForeignRegion filePath

  if opts ^. L.pretty
    then do
      forM_ (DVS.toList v) $ \w -> do
        forM_ [0..63] $ \i -> do
          if (w .>. i) .&. 1 == 1
            then IO.hPutChar IO.stdout '1'
            else IO.hPutChar IO.stdout '0'
          when (i `mod` 8 == 7) $ do
            if (i == 63)
              then IO.hPutChar IO.stdout '\n'
              else IO.hPutChar IO.stdout ' '
    else do
      forM_ (DVS.toList v) $ \w -> do
        forM_ [0..63] $ \i -> do
          if (w .>. i) .&. 1 == 1
            then IO.hPutChar IO.stdout '1'
            else IO.hPutChar IO.stdout '0'

  return ()

optsShowBits :: Parser ShowBitsOptions
optsShowBits = ShowBitsOptions
  <$> strOption
        (   long "file"
        <>  help "Separated Value file"
        <>  metavar "STRING"
        )
  <*> switch (long "pretty")

cmdShowBits :: Mod CommandFields (IO ())
cmdShowBits = command "show-bits"  $ flip info idm $ runShowBits <$> optsShowBits
