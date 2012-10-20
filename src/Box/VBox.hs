module Box.VBox
       ( vbManage
       , vbManageVM
       , vbManageVM_
       , vbManage_
       , vbSysProps
       , vbox
       ) where

import           Box.Text
import           Box.Types.VBox
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (isSpace)
import           Data.Map              (fromList)
import           Data.Text.Lazy        (Text)
import           Prelude               hiding (FilePath)
import           Shelly

default (Text)

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

manage'  :: [Text] -> ShIO Text
manage'  = run  "VBoxManage"
manage_' :: [Text] -> ShIO ()
manage_' = run_ "VBoxManage"
