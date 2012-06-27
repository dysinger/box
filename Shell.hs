module Shell ( module Shelly
             , homePath
             , status
             ) where

import qualified Data.Text.Lazy   as DTL
import           Prelude          hiding (FilePath)
import           Shelly
import           System.Directory

default (DTL.Text)

homePath :: ShIO FilePath
homePath = return . fromText . DTL.pack =<< liftIO getHomeDirectory

status :: DTL.Text -> ShIO a -> ShIO a
status msg shio = do
  echo msg
  x <- shio
  return x
