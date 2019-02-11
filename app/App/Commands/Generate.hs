{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.Generate
  ( cmdGenerate
  ) where

import Control.Lens
import Data.Generics.Product.Any
import Data.List
import Data.Semigroup            ((<>))
import Options.Applicative       hiding (columns)

import qualified App.Commands.Options.Type as Z
import qualified App.Gen                   as G
import qualified Hedgehog.Gen              as G
import qualified Hedgehog.Range            as R

printField :: String -> String
printField cs = if any invalid cs
  then escape cs
  else cs
  where invalid c = c == ',' || c == '\r' || c == '\n' || c == '"'
        escape    = ("\"" <>) . (<> "\"") . concatMap (\c -> if c == '"' then "\"\"" else [c])

runGenerate :: Z.GenerateOptions -> IO ()
runGenerate opts = do
  let fields  = opts ^. the @"fields"
  let rows    = opts ^. the @"rows"

  csv <- G.sample (G.list (R.singleton rows) (G.list (R.singleton fields) G.field)) :: IO [[String]]

  putStr $ unlines $ map (intercalate "," . map printField) csv

  return ()

optsGenerate :: Parser Z.GenerateOptions
optsGenerate = Z.GenerateOptions
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
