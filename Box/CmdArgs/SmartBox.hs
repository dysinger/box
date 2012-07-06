module Box.CmdArgs.SmartBox where

import Box.SmartBox
import Box.Text
import Box.Types

-- cmdargs imports
import System.Console.CmdArgs.Default
import System.Console.CmdArgs.Explicit hiding (mode)

-- shelly imports
import Prelude                         hiding (FilePath)
import Shelly                          hiding (shelly)

-- default to Text strings
import Data.Text.Lazy                  (Text)
default (Text)

dispatch :: Cmd -> ShIO ()
dispatch SmartBoxHelp{..}  = echo . toTxt $ helpText [] def mode
dispatch SmartBoxSetup{..} = setup
dispatch _                 = echo "WTFBBQ?!"

mode :: Mode Cmd
mode =
  def { modeGroupFlags = toGroup [ flagHelpSimple (\c -> c) ]
      , modeGroupModes = toGroup [ setupMode ]
      , modeHelp       = "SmartOS/VirtualBox Management"
      , modeNames      = ["smartbox"]
      , modeValue      = SmartBoxHelp }
  where
    setupMode =
      def { modeHelp  = "SmartOS/VirtualBox Setup"
          , modeNames = ["setup"]
          , modeValue = SmartBoxSetup }
