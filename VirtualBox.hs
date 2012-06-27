module VirtualBox
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

import qualified Data.ByteString.Char8 as DBC
import           Data.Char             (isSpace)
import           Data.Data
import           Data.Map              (Map, fromList)
import qualified Data.Text.Lazy        as DTL
import           Prelude               hiding (FilePath)
import           Shell

default (DTL.Text)

-----------------------------------------------------------------------------

data VBoxVM = VBoxVM { ident :: DTL.Text }
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

-- TODO get off DBC! and back to DTL!

vbSysProps :: ShIO VBoxProperties
vbSysProps = do
  p <- vbManage List [ "systemproperties" ]
  return $ fromList $ map props $ lines $ DTL.unpack p
  where props     = tuple . map clean . split . pack
        tuple     = (\(k:v:_) -> (k, v))
        clean     = trim . unpack
        split     = DBC.split ':'
        pack      = DBC.pack
        unpack    = DBC.unpack
        trim      = dropSpace . dropSpace
        dropSpace = reverse . dropWhile isSpace

vbox :: Bool -> [DTL.Text] -> ShIO ()
vbox _headless@True  = run_ "VBoxHeadless"
vbox _headless@False = run_ "VirtualBox"

vbManage  :: VBoxManageCmd -> [DTL.Text] -> ShIO DTL.Text
vbManage  vCmd args = manage'  ((toLower vCmd):args)
vbManage_ :: VBoxManageCmd -> [DTL.Text] -> ShIO ()
vbManage_ vCmd args = manage_' ((toLower vCmd):args)

vbManageVM  :: VBoxVM -> VBoxManageVmCmd -> [DTL.Text] -> ShIO DTL.Text
vbManageVM  VBoxVM{..} vCmd args = manage'  ((toLower vCmd):ident:args)
vbManageVM_ :: VBoxVM -> VBoxManageVmCmd -> [DTL.Text] -> ShIO ()
vbManageVM_ VBoxVM{..} vCmd args = manage_' ((toLower vCmd):ident:args)

-----------------------------------------------------------------------------

manage'  :: [DTL.Text] -> ShIO DTL.Text
manage'  = run  "VBoxManage"
manage_' :: [DTL.Text] -> ShIO ()
manage_' = run_ "VBoxManage"

toText :: forall a. Show a => a -> DTL.Text
toText = DTL.pack . show

toLower :: forall a. Show a => a -> DTL.Text
toLower = DTL.toLower . toText
