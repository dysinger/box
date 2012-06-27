module SmartBox.Shell where

import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as T
import qualified Prelude                 as P
import           Prelude                 hiding (FilePath)
import           Shelly
import           System.Directory

default (Text)

-----------------------------------------------------------------------------

homePath :: ShIO FilePath
homePath = return . fromText . T.pack =<< liftIO getHomeDirectory

status :: [Text] -> ShIO a -> ShIO a
status msg shio = do
  let msg' = T.unwords msg
  echo msg'
  x <- shio
  return x
