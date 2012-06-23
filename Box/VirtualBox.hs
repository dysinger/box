module Box.VirtualBox
       ( VBoxManageCmd(..)
       , VBoxManageVmCmd(..)
       , VBoxVM(..)
       , manage
       , manage_
       , manageVM
       , manageVM_
       , properties
       ) where

import qualified Data.ByteString.Char8 as DBC
import           Data.Char             (isSpace)
import           Data.Data
import           Data.Map              (Map, fromList)
import qualified Data.Text.Lazy        as DTL
import           Prelude               hiding (FilePath)
import           Shelly

default (DTL.Text)

-----------------------------------------------------------------------------

data VBoxVM = VBoxVM { vmIdent :: DTL.Text }
            deriving (Data, Typeable, Eq)

instance Show VBoxVM where
  show = show . vmIdent

data VBoxManageCmd = CreateHD
                   | CreateIDE
                   | CreateVM
                   | List
                   deriving (Data, Show, Typeable, Eq)

data VBoxManageVmCmd = ModifyVM
                     | ShowInfo
                     | StorageAttach
                     deriving (Data, Show, Typeable, Eq)

-----------------------------------------------------------------------------

properties :: ShIO (Map String String)
properties = do
  p <- manage List [ "systemproperties" ]
  return $ fromList $ map props $ lines $ DTL.unpack p
  where props     = tuple . map clean . split . pack
        tuple     = (\(k:v:_) -> (k, v))
        clean     = trim . unpack
        split     = DBC.split ':'
        pack      = DBC.pack
        unpack    = DBC.unpack
        trim      = dropSpace . dropSpace
        dropSpace = reverse . dropWhile isSpace

-----------------------------------------------------------------------------

vbox :: Bool -> [DTL.Text] -> ShIO ()
vbox _headless@True  = run_ "VBoxHeadless"
vbox _headless@False = run_ "VirtualBox"

manage :: VBoxManageCmd -> [DTL.Text] -> ShIO DTL.Text
manage vboxCmd args = manage' ((toLower vboxCmd):args)

manage_ :: VBoxManageCmd -> [DTL.Text] -> ShIO ()
manage_ vboxCmd args = manage_' ((toLower vboxCmd):args)

manageVM :: VBoxVM -> VBoxManageVmCmd -> [DTL.Text] -> ShIO DTL.Text
manageVM vm vboxCmd args = manage' ((toLower vboxCmd):(toText vm):args)

manageVM_ :: VBoxVM -> VBoxManageVmCmd -> [DTL.Text] -> ShIO ()
manageVM_ vm vboxCmd args = manage_' ((toLower vboxCmd):(toText vm):args)

-----------------------------------------------------------------------------
-- Internal
-----------------------------------------------------------------------------

manage' :: [DTL.Text] -> ShIO DTL.Text
manage' = run "VBoxManage"

manage_' :: [DTL.Text] -> ShIO ()
manage_' = run_ "VBoxManage"

-----------------------------------------------------------------------------

toText :: forall a. Show a => a -> DTL.Text
toText = DTL.pack . show

toLower :: forall a. Show a => a -> DTL.Text
toLower = DTL.toLower . toText
