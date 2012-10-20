module Box.Types.VBox where

import Data.Data
import Data.Map       (Map)
import Data.Text.Lazy (Text)
import Prelude        hiding (FilePath)
import Shelly

default (Text)

data VBoxVM = VBoxVM { vmIdent   :: Text
                     , vmDirPath :: FilePath }
            deriving (Data, Show, Typeable, Eq)

data VBoxManageCmd = CreateHD
                   | CreateVM
                   | List
                   deriving (Data, Show, Typeable, Eq)

data VBoxManageVmCmd = ModifyVM
                     | ShowVMInfo
                     | StorageAttach
                     | StorageCtl
                     deriving (Data, Show, Typeable, Eq)

type VBoxProperties = Map String String
