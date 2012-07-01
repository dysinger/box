module Box.CmdArgs.SmartBox where

import Box.Types.CmdArgs
import Data.Text.Lazy                  (Text)
import System.Console.CmdArgs.Explicit

default (Text)

smartBoxMode :: Mode Cmd
smartBoxMode =
  Mode { modeArgs       = ([], Nothing)
       , modeCheck      = (\x -> Right x)
       , modeGroupFlags = toGroup []
       , modeGroupModes = toGroup [ setupMode ]
       , modeHelp       = "SmartOS / VirtualBox Management"
       , modeHelpSuffix = []
       , modeNames      = ["smartbox"]
       , modeReform     = (\x -> Just [show x])
       , modeValue      = SmartBoxSetup }
  where
    setupMode =
      Mode { modeArgs       = ([], Nothing)
           , modeCheck      = (\x -> Right x)
           , modeGroupFlags = toGroup []
           , modeGroupModes = toGroup []
           , modeHelp       = "SmartOS / VirtualBox Instance Setup"
           , modeHelpSuffix = []
           , modeNames      = ["setup"]
           , modeReform     = (\x -> Just [show x])
           , modeValue      = SmartBoxSetup }
