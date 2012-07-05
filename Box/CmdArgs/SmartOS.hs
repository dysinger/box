module Box.CmdArgs.SmartOS where

import Box.SmartOS
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
dispatch SmartOSHelp{..}      = echo . toTxt $ helpText [] def mode
dispatch SmartOSDownload{..}  = download
dispatch SmartOSBootstrap{..} = bootstrap
dispatch _                    = echo "WTFBBQ?!"

mode :: Mode Cmd
mode =
  def { modeGroupFlags = toGroup [ flagHelpSimple (\c -> c) ]
      , modeGroupModes = toGroup [ downloadMode, bootstrapMode ]
      , modeHelp       = "SmartOS Management"
      , modeNames      = ["smartos"]
      , modeValue      = SmartOSHelp }
  where
    downloadMode =
      def { modeHelp  = "SmartOS Platform Download"
          , modeNames = ["download"]
          , modeValue = SmartOSHelp }
    bootstrapMode =
      def { modeGroupFlags =
               toGroup [ flagOpt "127.0.0.1" ["host","h"] upd "HOST"
                         "Host or IP for SSH"
                       , flagOpt "22"        ["port","p"] upd "PORT"
                         "TCP Port for SSH"
                       , flagOpt "root"      ["user","U"] upd "USER"
                         "User for SSH"
                       , flagOpt "root"      ["pass","P"] upd "PASS"
                         "Password for SSH"
                       ]
          , modeHelp       = "SmartOS Platform Bootstrap"
          , modeNames      = ["bootstrap"]
          , modeValue      = SmartOSHelp }
    upd _str val =
      -- TODO validate hostname | IPv4 | IPv6 addr
      -- TODO validate port
      -- TODO validate user
      -- TODO validate password
      Right val
