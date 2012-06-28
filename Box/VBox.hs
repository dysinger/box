module Box.VBox
       ( VBoxManageCmd(..)
       , VBoxManageVmCmd(..)
       , VBoxVM(..)
       , vbox
       , vbManage
       , vbManage_
       , vbManageVM
       , vbManageVM_
       , vbSysProps
       ) where

import qualified Data.ByteString.Char8 as BC
import           Data.Char             (isSpace)
import           Data.Data
import           Data.Map              (Map, fromList)
import           Data.Text.Lazy        (Text)
import qualified Prelude               as P
import           Prelude               hiding (FilePath)
import           Shelly
import           Box.Text

default (Text)

-----------------------------------------------------------------------------

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

vbSysProps :: ShIO VBoxProperties
vbSysProps = do
  p <- vbManage List [ "systemproperties" ]
  return $ fromList $ map props $ lines $ txtToStr p
  where props     = tuple . map clean . split . pack
        tuple     = (\(k:v:_) -> (k, v))
        clean     = trim . unpack
        split     = BC.split ':'
        pack      = BC.pack
        unpack    = BC.unpack
        trim      = dropSpace . dropSpace
        dropSpace = reverse . dropWhile isSpace

vbox :: Bool -> [Text] -> ShIO ()
vbox _headless@True  = run_ "VBoxHeadless"
vbox _headless@False = run_ "VirtualBox"

vbManage  :: VBoxManageCmd -> [Text] -> ShIO Text
vbManage  vCmd args = manage'  ((toLowerTxt vCmd):args)
vbManage_ :: VBoxManageCmd -> [Text] -> ShIO ()
vbManage_ vCmd args = manage_' ((toLowerTxt vCmd):args)

vbManageVM  :: VBoxVM -> VBoxManageVmCmd -> [Text] -> ShIO Text
vbManageVM  VBoxVM{..} vCmd args = manage'  ((toLowerTxt vCmd):vmIdent:args)
vbManageVM_ :: VBoxVM -> VBoxManageVmCmd -> [Text] -> ShIO ()
vbManageVM_ VBoxVM{..} vCmd args = manage_' ((toLowerTxt vCmd):vmIdent:args)

-----------------------------------------------------------------------------

manage'  :: [Text] -> ShIO Text
manage'  = run  "VBoxManage"
manage_' :: [Text] -> ShIO ()
manage_' = run_ "VBoxManage"
