module App.Commands.Query
  ( cmdQuery
  ) where

import Options.Applicative

runQuery :: IO ()
runQuery = return ()

cmdQuery :: Mod CommandFields (IO ())
cmdQuery = command "query"  (info (pure runQuery) idm)
