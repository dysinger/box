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
  createOrUpdate SmartBox { sbVm         = vm
                          , sbVmDiskPath = diskPath
                          , sbIsoPath    = isoPath'
                          , sbPlatform   = so }

-- | create or update a SmartBox depending on the current state
createOrUpdate :: SmartBox -> ShIO ()
createOrUpdate sb@SmartBox{..} =
  do vbManageVM_ sbVm ShowVMInfo []
     update sb
  `catch_sh`
  (\(_e :: SomeException) -> create sb)

-- | create a new SmartBox
create :: SmartBox -> ShIO ()
create sb@SmartBox{..} = do
  status ["Creating a SmartBox", vmDirPath']
    $ vbManage_ CreateVM [ "--name", vmIdent sbVm
                         , "--ostype", "OpenSolaris_64"
                         , "--register" ]
  status ["Setting CPUs to 2 and Memory to 4096MB"]
    $ vm ModifyVM [ "--cpus", "2", "--memory", "4096" ]
  status ["Adding an IDE controller to SmartOS"]
    $ vm StorageCtl [ "--add" , "ide", "--name", "IDE Controller" ]
  status ["Creating 40GB disk", vmDiskPath]
    $ vbManage_ CreateHD [ "--filename", vmDiskPath, "--size", "40960" ]
  status ["Attaching 40GB disk", vmDiskPath]
    $ vm StorageAttach [ "--device", "0"
                       , "--medium", vmDiskPath
                       , "--port", "0"
                       , "--storagectl" , "IDE Controller"
                       , "--type", "hdd" ]
  status ["Setting disk boot order"]
    $ vm ModifyVM [ "--boot1", "dvd", "--boot2", "disk", "--boot3", "none" ]
  status ["Setting 1st net adapter to NAT @ ssh on port 2222 @ localhost"]
    $ vm ModifyVM [ "--natpf1", "SSH,tcp,,2222,,22" ]
  status ["Setting 2nd net adapter to bridged on host interface wlan0"]
    $ vm ModifyVM [ "--nic2", "bridged", "--bridgeadapter2", strToTxt "wlan0" ]
  status ["Setting 3nd net adapter to bridged on host interface wlan0"]
    $ vm ModifyVM [ "--nic3", "bridged", "--bridgeadapter3", strToTxt "wlan0" ]
  status ["Setting 4th net adapter to bridged on host interface wlan0"]
    $ vm ModifyVM [ "--nic4", "bridged", "--bridgeadapter4", strToTxt "wlan0" ]
  update sb
  where vm         = vbManageVM_            $ sbVm
        vmDirPath' = fpToTxt . vmDirPath    $ sbVm
        vmDiskPath = fpToTxt                $ sbVmDiskPath

-- | update an existing SmartBox
update :: SmartBox -> ShIO ()
update SmartBox{..} = do
  status ["Attaching SmartOS ISO", fpToTxt sbIsoPath]
    $ vbManageVM_ sbVm StorageAttach [ "--device", "0"
                                     , "--medium", toTextIgnore sbIsoPath
                                     , "--port", "1"
                                     , "--storagectl", "IDE Controller"
                                     , "--type", "dvddrive" ]
