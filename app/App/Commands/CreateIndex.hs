module App.Commands.CreateIndex
  ( cmdCreateIndex
  ) where

import Options.Applicative

runCreateIndex :: String -> IO ()
runCreateIndex _ = return ()

cmdCreateIndex :: Mod CommandFields (IO ())
cmdCreateIndex = command "create-index" (info (runCreateIndex <$> argument str idm) idm)
