module Box.CmdArgs where

import qualified Box.CmdArgs.SmartOS             as SmartOS
import qualified Box.CmdArgs.SmartBox            as SmartBox
import           Box.Shelly
import           Box.Types
import           Box.Text

-- cmdargs imports
import           System.Console.CmdArgs.Default
import           System.Console.CmdArgs.Explicit hiding (mode)

-- shelly imports
import           Prelude                         hiding (FilePath)
import           Shelly                          hiding (shelly, command, cmd)

-- default to Text strings
import           Data.Text.Lazy                  (Text)
default (Text)

main :: IO ()
main = processArgs mode >>= shelly . dispatch

-- TODO defer to sub-modules for dispatching specific commands
dispatch :: Cmd -> ShIO ()
dispatch Help{..}                 = echo . toTxt $ helpText [] def mode
dispatch cmd@SmartOSDownload{..}  = SmartOS.dispatch  cmd
dispatch cmd@SmartOSHelp{..}      = SmartOS.dispatch  cmd
dispatch cmd@SmartOSBootstrap{..} = SmartOS.dispatch  cmd
dispatch cmd@SmartBoxHelp{..}     = SmartBox.dispatch cmd
dispatch cmd@SmartBoxSetup{..}    = SmartBox.dispatch cmd

mode :: Mode Cmd
mode =
  def { modeGroupFlags = toGroup [ flagHelpSimple (\c -> c)
                                 , flagVersion (\c -> c) ]
      , modeGroupModes = toGroup [ SmartOS.mode, SmartBox.mode ]
      , modeHelp       = "Box Management"
      , modeHelpSuffix = [ "TODO: Provide a paragraph or two on "
                         , "how this app is supposed to be used." ]
      , modeNames      = ["box"] }
