module Box.SmartBox (setup) where

import Box.Shelly
import Box.SmartOS
import Box.Text
import Box.Types
import Box.VBox

import Control.Exception
import Data.Map          hiding (null, update)

-- shelly imports
import Prelude           hiding (FilePath)
import Shelly            hiding (shelly)

-- default to Text strings
import Data.Text.Lazy    (Text)
default (Text)

-- | Setup a SmartBox with 4GB ram & 40GB disk for SmartOS
setup :: ShIO ()
setup = do
  -- deps
  download
  -- setup
  so    <- platform
  props <- vbSysProps
  home  <- homePath
  let diskPath    = vmDirPath' </> ("zones.vdi" :: FilePath)
      isoPath'    = isoPath so (isoDirPath home)
      vmBasePath  = findWithDefault "vm" "Default machine folder" props
      vmDirPath'  = vmBasePath </> (txtToStr . vmIdent $ vm)
      vm          = VBoxVM { vmIdent   = "smartbox"
                           , vmDirPath = vmDirPath' }
  maybeCreate SmartBox { sbVm         = vm
                       , sbVmDiskPath = diskPath
                       , sbIsoPath    = isoPath'
                       , sbPlatform   = so }

-- | create or update a SmartBox depending on the current state
maybeCreate :: SmartBox -> ShIO ()
maybeCreate sb@SmartBox{..} = do
  vbManageVM_ sbVm ShowVMInfo []
  update sb
  `catch_sh`
  (\(_e :: SomeException) -> do
      create sb
      update sb)

-- | create a new SmartBox
create :: SmartBox -> ShIO ()
create SmartBox{..} = do
  status ["Creating VM"] $ do
    vbManage_ CreateVM [ "--name", vmIdent sbVm
                       , "--ostype", "OpenSolaris_64"
                       , "--register" ]
    vbManageVM_ sbVm ModifyVM [ "--cpus", "2"
                              , "--memory", "4096" ]
  status ["Setting up network"] $ do
    vbManageVM_ sbVm ModifyVM [ "--natpf1", "SSH,tcp,,2222,,22" ]
  status ["Setting up disk controller"] $ do
    vbManageVM_ sbVm StorageCtl [ "--add" , "sata"
                                , "--name", "SATA Controller" ]
  status ["Making a 40GB 'zones' disk"] $ do
    vbManage_ CreateHD [ "--filename", fpToTxt $ sbVmDiskPath
                       , "--size", "40960" ]
    vbManageVM_ sbVm StorageAttach [ "--device", "0"
                                   , "--medium", fpToTxt $ sbVmDiskPath
                                   , "--port", "0"
                                   , "--storagectl" , "SATA Controller"
                                   , "--type", "hdd" ]

update :: SmartBox -> ShIO ()
update SmartBox{..} = do
  status ["Attaching ISO as DVD"] $ do
    vbManageVM_ sbVm StorageAttach [ "--device", "0"
                                   , "--medium", toTextIgnore sbIsoPath
                                   , "--port", "1"
                                   , "--storagectl", "SATA Controller"
                                   , "--type", "dvddrive" ]
  status ["Setting boot from DVD"] $ do
    vbManageVM_ sbVm ModifyVM [ "--boot1", "dvd"
                              , "--boot2", "none"
                              , "--boot3", "none" ]
