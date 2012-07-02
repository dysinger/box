module Box.Types.CmdArgs where

import Data.Data
import Data.Text.Lazy                  (Text)
import System.Console.CmdArgs.Default

default (Text)

data Cmd = Help
           -- smartos
         | SmartOSDownload
           -- smartbox
         | SmartBoxSetup
         deriving (Data, Eq, Show, Typeable)

instance Default Cmd where
  def = Help
