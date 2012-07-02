module Box.CmdArgs.SmartOS where

import Box.Types.CmdArgs
import Data.Text.Lazy                  (Text)
import System.Console.CmdArgs.Explicit hiding (mode)

default (Text)

smartOSMode :: Mode Cmd
smartOSMode =
  Mode { modeArgs       = ([], Nothing)
       , modeCheck      = (\x -> Right x)
       , modeGroupFlags = toGroup []
       , modeGroupModes = toGroup [ downloadMode ]
       , modeHelp       = "SmartOS Management"
       , modeHelpSuffix = []
       , modeNames      = ["smartos"]
       , modeReform     = (\x -> Just [show x])
       , modeValue      = SmartOSDownload }
  where
    downloadMode =
      Mode { modeArgs       = ([], Nothing)
           , modeCheck      = (\x -> Right x)
           , modeGroupFlags = toGroup []
           , modeGroupModes = toGroup []
           , modeHelp       = "SmartOS Platform Download"
           , modeHelpSuffix = []
           , modeNames      = ["download"]
           , modeReform     = (\x -> Just [show x])
           , modeValue      = SmartOSDownload }
