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
       , modeHelp       = "SmartBox Management"
       , modeHelpSuffix = []
       , modeNames      = ["smartbox"]
       , modeReform     = (\x -> Just [show x])
       , modeValue      = Help }
  where
    setupMode =
      Mode { modeArgs       = ([], Nothing)
           , modeCheck      = (\x -> Right x)
           , modeGroupFlags = toGroup []
           , modeGroupModes = toGroup []
           , modeHelp       = "SmartBox Setup"
           , modeHelpSuffix = []
           , modeNames      = ["setup"]
           , modeReform     = (\x -> Just [show x])
           , modeValue      = SmartBoxSetup }
