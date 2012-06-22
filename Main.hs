import qualified Box.SmartBox                    as SmartBox
import qualified Box.Provider.Joyent             as Joyent
import           System.Console.CmdArgs.Explicit

main :: IO ()
main = do
  processArgs ms >>= putStrLn . show
  where
    ms = modes "smartos" [] "SmartOS Management" [ SmartBox.mode
                                                 , Joyent.mode ]
