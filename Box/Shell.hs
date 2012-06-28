module Box.Shell where

import           Data.Int
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
  verbosity' <- liftIO getVerbosity
  case verbosity' of
    Normal -> echo_n (format msg') >> m >>= \x -> do echo done ; return x
    _      -> trace msg'           >> m >>=                      return
  where
    diff       = (-) limit . T.length
    done       = "[DONE]"
    format txt = T.append txt $ T.take (diff txt) spaces
    limit      = (80 :: Int64) - (T.length done)
    msg'       = T.take limit . T.unwords $ msg
    spaces     = T.fromChunks . repeat    $ " "
