module Box.Types.SmartOS where

import Data.Data
import Data.Text.Lazy (Text)
import Prelude        hiding (FilePath)

default (Text)

data SmartOS = SmartOS { isoName :: Text
                       , isoMd5  :: Text }
             deriving (Data, Show, Typeable, Eq)
