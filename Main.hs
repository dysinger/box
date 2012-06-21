{-# LANGUAGE RecordWildCards #-}

import qualified Box.SmartBox            as SmartBox
import           Box.Types
import           System.Console.CmdArgs
import           System.Environment

main :: IO ()
main = do
  y <- (if null x then withArgs ["--help"] else id) $ cmdArgsRun modes'
  dispatch y
  args <- getArgs
  where
    modes'      = cmdArgsMode $ modes
                  [ VBox { sync = def &= help "Syncronize Releases" }
                  , Joyent
                  ]
                  &= program name'
                  &= help about'
                  &= summary (info' ++ ", " ++ copyright')
                  &= helpArg       [ explicit, name "help",    name "h" ]
                  &= verbosityArgs [ explicit, name "Verbose", name "V" ] []
                  &= versionArg    [ explicit
                                   , name "version"
                                   , name "v"
                                   , summary info' ]
    name'       = "box"
    version'    = "0.1.0.0"
    info'       = name' ++ " version " ++ version'
    about'      = "A command line application to manage boxes."
    copyright'  = "Copyright © 2012 Positive Inertia, LLC."

dispatch :: Command -> IO ()
dispatch v@VBox{..} = SmartBox.dispatch v
dispatch _ = return ()
