import qualified Box.Joyent                      as Joyent
import qualified Box.SmartBox                    as SmartBox
import           Data.Data
import           System.Console.CmdArgs.Explicit

-----------------------------------------------------------------------------

data Command = Joyent
             | LaunchJoyent
             | DestroyJoyent
             | VirtualBox
             | SmartBox
             | CreateSmartBox
             | UpdateSmartBox
             | DestroySmartBox
             deriving (Data, Typeable, Show, Eq)

-----------------------------------------------------------------------------

main :: IO ()
main = do
  processArgs ms >>= putStrLn . show
  where
    ms = modes "smartos" [] "SmartOS Management" [ SmartBox.mode
                                                 , Joyent.mode ]
