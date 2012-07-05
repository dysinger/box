module Box.Shelly where

import           Control.Monad.IO.Class
import           Data.Int
import qualified Data.Text.Lazy         as T
import           System.Directory

-- cmdargs imports
import           System.Console.CmdArgs

-- shelly imports
import           Prelude                hiding (FilePath)
import           Shelly                 hiding (shelly)
import qualified Shelly                 as S

-- default to Text strings
import           Data.Text.Lazy         (Text)
default (Text)

shelly :: ShIO a -> IO a
shelly m = getVerbosity >>= S.shelly . flip withVerbosity m

withVerbosity :: Verbosity -> ShIO a -> ShIO a
withVerbosity Quiet  = silently  . print_stdout False . print_commands False
withVerbosity Normal =             print_stdout False . print_commands False
withVerbosity Loud   = verbosely . print_stdout True  . print_commands True

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
