module Box.Types.CmdArgs where

import Data.Data

-- cmdargs imports
import           System.Console.CmdArgs.Default
import           System.Console.CmdArgs.Explicit hiding (mode)

-- default to Text strings
import           Data.Text.Lazy                  (Text)
default (Text)

data Cmd = Help
           -- smartos
         | SmartOSHelp
         | SmartOSDownload
         | SmartOSBootstrap { host :: String
                            , port :: Int
                            , user :: String
                            , pass :: String }
           -- smartbox
         | SmartBoxHelp
         | SmartBoxSetup
         deriving (Data, Eq, Show, Typeable)

instance Default Cmd where
  def = Help

instance Default (Mode Cmd) where
  def = Mode { modeArgs       = ([], Nothing)
             , modeCheck      = (\x -> Right x)
             , modeGroupFlags = toGroup []
             , modeGroupModes = toGroup []
             , modeHelp       = ""
             , modeHelpSuffix = []
             , modeNames      = []
             , modeReform     = (\x-> Just [show x])
             , modeValue      = def }
