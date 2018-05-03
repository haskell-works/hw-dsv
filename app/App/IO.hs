module App.IO where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import System.IO                    (Handle)

import qualified System.IO as IO

openOutputFile :: MonadResource m => FilePath -> Maybe Int -> m (ReleaseKey, Handle)
openOutputFile "-" _ = allocate (return IO.stdout) (const (return ()))
openOutputFile filePath maybeBufferSize = allocate open close
  where open  = do
          handle <- IO.openFile filePath IO.WriteMode
          forM_ maybeBufferSize $ \bufferSize -> do
            liftIO $ IO.hSetBuffering handle (IO.BlockBuffering (Just (64 * 1024 * 1024)))
          return handle
        close = IO.hClose
