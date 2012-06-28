module Box.Shell where

import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as T
import qualified Prelude                 as P
import           Prelude                 hiding (FilePath)
import           Shelly
import           System.Console.CmdArgs
import           System.Directory

default (Text)

-----------------------------------------------------------------------------

homePath :: ShIO FilePath
homePath = return . fromText . T.pack =<< liftIO getHomeDirectory

status :: [Text] -> ShIO a -> ShIO a
status msg m = do
  let msg' = T.unwords msg
  verbosity' <- liftIO getVerbosity
  case verbosity' of
    Quiet  -> trace msg'  >> m >>= return
    Normal -> echo_n msg' >> m >>= \x -> do echo " [DONE]" ; return x
    Loud   -> echo msg'   >> m >>= return
