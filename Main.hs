import qualified Box.Joyent                      as Joyent
import qualified Box.SmartBox                    as SmartBox
import           Data.Data
import           System.Console.CmdArgs.Explicit

-----------------------------------------------------------------------------

data Command = SmartBox
             deriving (Data, Typeable, Show, Eq)

-----------------------------------------------------------------------------

-- TODO get a nice looking help working again
-- TODO make shelly shut up about the console output

main :: IO ()
main = do
  processArgs ms >>= putStrLn . show
  SmartBox.main
  where
    ms = modes "smartos" [] "SmartOS Management" [ SmartBox.mode
                                                 , Joyent.mode ]
