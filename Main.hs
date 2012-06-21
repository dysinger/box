{-# LANGUAGE RecordWildCards #-}

import qualified Box.SmartBox            as SmartBox
import qualified Data.Char               as DC
import           System.Environment

main :: IO ()
main = getArgs >>= dispatch . lower

lower :: [String] -> [String]
lower (arg:args) = (map DC.toLower arg) : args

dispatch :: [String] -> IO ()
dispatch ("smart":args) = withArgs args $ SmartBox.main
dispatch ("help":_) = putStrLn "box [COMMAND] ... [OPTIONS] ..."
dispatch _ = return ()
