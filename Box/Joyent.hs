module Box.Joyent where

import           System.Console.CmdArgs.Explicit hiding (mode)
import qualified System.Console.CmdArgs.Explicit as SCCE

mode :: Mode [(String,String)]
mode = SCCE.mode "joyent" [] "Joyent Management" (flagArg (upd "file") "FILE")
      [ flagOpt "world" ["hello","h"]    (upd "world")    "WHO" "World argument"
      , flagReq         ["greeting","g"] (upd "greeting") "MSG" "Greeting to give"
      , flagHelpSimple (("help",""):)
      ]
    where upd msg x v = Right $ (msg,x):v
