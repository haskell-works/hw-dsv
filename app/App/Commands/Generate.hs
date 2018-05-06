{-# LANGUAGE BangPatterns #-}

module App.Commands.Generate
  ( cmdGenerate
  ) where

import App.Commands.Options.Type
import Control.Lens
import Data.List
import Data.Semigroup            ((<>))
import Options.Applicative       hiding (columns)

import qualified App.Commands.Options.Lens as L
import qualified App.Gen                   as G
import qualified Data.ByteString.Builder   as B
import qualified Hedgehog.Gen              as G
import qualified Hedgehog.Range            as R

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
