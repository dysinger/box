module Box.Types.CmdArgs where

import Data.Data
import Data.Text.Lazy                  (Text)
import System.Console.CmdArgs.Default

default (Text)

data Cmd = Help
         | SmartPlatformSync
         | SmartBoxSetup
         | SmartOSBootstrap { smart_os_host :: Maybe String
                            , smart_os_port :: Maybe Int }
         deriving (Data, Eq, Show, Typeable)

instance Default Cmd where
  def = Help
