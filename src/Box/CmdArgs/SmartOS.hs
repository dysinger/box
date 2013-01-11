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
  def { modeGroupFlags = toGroup [ flagHelpSimple id ]
      , modeGroupModes = toGroup [ downloadMode, bootstrapMode ]
      , modeHelp       = "SmartOS Management"
      , modeNames      = ["smartos"]
      , modeValue      = SmartOSHelp }
  where
    downloadMode =
      def { modeHelp  = "SmartOS Platform Download"
          , modeNames = ["download"]
          , modeValue = SmartOSDownload }
    bootstrapMode =
      def { modeGroupFlags =
               toGroup [ flagReq ["host", "h"] (upd "host") "HOST" "Host or IP for SSH"
                       , flagReq ["port", "p"] (upd "port") "PORT" "TCP Port for SSH"
                       , flagReq ["user", "U"] (upd "user") "USER" "User for SSH"
                       , flagReq ["pass", "P"] (upd "pass") "PASS" "Password for SSH"
                       ]
          , modeHelp       = "SmartOS Platform Bootstrap"
          , modeNames      = ["bootstrap"]
          , modeValue      = SmartOSBootstrap { host = "127.0.0.1"
                                              , port = 22
                                              , user = "root"
                                              , pass = "root" }
          }
    upd :: Text -> String -> Cmd -> Either String Cmd
    upd "host" str val   = Right $ val { host = strToTxt str }
    upd "port" str val   = Right $ val { port = read str }
    upd "user" str val   = Right $ val { user = strToTxt str }
    upd "pass" str val   = Right $ val { pass = strToTxt str }
    upd _      _   _     = Left "WTFBBQ?!"
