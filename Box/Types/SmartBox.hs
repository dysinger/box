module Box.Types.SmartBox where

import Box.Types.SmartOS
import Box.Types.VBox
import Data.Data
import Data.Text.Lazy    (Text)
import Prelude           hiding (FilePath)
import Shelly            (FilePath)

default (Text)

data SmartBox = SmartBox { sbVm         :: VBoxVM
                         , sbVmDiskPath :: FilePath
                         , sbIsoPath    :: FilePath
                         , sbPlatform   :: SmartOS }
         deriving (Data, Show, Typeable, Eq)
