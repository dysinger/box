module Box.CmdArgs (main) where

import Box.CmdArgs.SmartBox
import Box.CmdArgs.SmartOS
import Box.SmartBox
import Box.SmartOS
import Box.Types
import Control.Exception
import Data.Text.Lazy                  (Text)
import Prelude                         hiding (FilePath)
import Shelly
import System.Console.CmdArgs          hiding (help, modes)
import System.Console.CmdArgs.Explicit hiding (mode, modes)

default (Text)

-- | Main entry point & processor of command line arguments
main :: IO ()
main = do
  command'   <- processArgs mode
  verbosity' <- getVerbosity
  if command' == Help
    then putStrLn . show $ helpText [] HelpFormatDefault mode
    else shelly
         $ case verbosity' of
           Quiet  -> silently  . print_stdout False . print_commands False
           Normal -> sub       . print_stdout False . print_commands False
           Loud   -> verbosely . print_stdout True  . print_commands True
         $ platform >>= dispatch' command'
  where
    -- TODO find a way to not have knowledge of every command (module dispatch)
    dispatch' SmartOSDownload = download
    dispatch' SmartBoxSetup   = setup
    dispatch' _               = throw $ Ex "Problem with Main.main dispatch"

-- | Consolidate all the modes of this application
mode :: Mode Cmd
mode =
  Mode { modeArgs       = ([], Nothing)
       , modeCheck      = (\x -> Right x)
       , modeGroupFlags = toGroup [ flagHelpSimple (\c -> c)
                                  , flagVersion (\c -> c) ]
       , modeGroupModes = toGroup [ smartOSMode, smartBoxMode ]
       , modeHelp       = "Box Management"
       , modeHelpSuffix = [ "TODO: Provide a paragraph or two on "
                          , "how this app is supposed to be used." ]
       , modeNames      = ["box"]
       , modeReform     = (\x -> Just [show x])
       , modeValue      = Help }
