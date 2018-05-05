{-# LANGUAGE BangPatterns #-}

module App.Commands.Generate
  ( cmdGenerate
  ) where

import App.Commands.Options.Type
import Control.Lens
import Control.Monad
import Data.Char
import Data.Ix
import Data.List
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.RankSelect.CsPoppy
import HaskellWorks.Data.Sv.Char
import HaskellWorks.Data.Sv.Load
import Hedgehog
import Options.Applicative                  hiding (columns)

import qualified App.Commands.Options.Lens        as L
import qualified App.Gen                          as G
import qualified Data.ByteString.Builder          as B
import qualified Data.Vector.Storable             as DVS
import qualified HaskellWorks.Data.Sv.Cursor.Lens as LC
import qualified Hedgehog.Gen                     as G
import qualified Hedgehog.Range                   as R
import qualified System.IO                        as IO

printField :: String -> String
printField cs = if any invalid cs
  then escape cs
  else cs
  where invalid c = c == ',' || c == '\r' || c == '\n' || c == '"'
        escape    = ("\"" <>) . (<> "\"") . concatMap (\c -> if c == '"' then "\"\"" else [c])

runGenerate :: GenerateOptions -> IO ()
runGenerate opts = do
  let fields  = opts ^. L.fields
  let rows    = opts ^. L.rows

  csv <- G.sample (G.list (R.singleton rows) (G.list (R.singleton fields) (G.field))) :: IO [[String]]

  putStr $ unlines $ map (intercalate "," . map printField) csv

  return ()

optsGenerate :: Parser GenerateOptions
optsGenerate = GenerateOptions
  <$> option auto
        (   long "fields"
        <>  help "Number of fields"
        <>  metavar "INT"
        )
  <*> option auto
        (   long "rows"
        <>  help "Number of rows"
        <>  metavar "INT"
        )

cmdGenerate :: Mod CommandFields (IO ())
cmdGenerate = command "generate"  $ flip info idm $ runGenerate <$> optsGenerate
