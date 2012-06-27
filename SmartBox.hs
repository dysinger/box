import           Control.Exception      (SomeException)
import           Data.Data
import           Data.Map               (findWithDefault)
import           Data.Text.Lazy         (Text)
import           System.Console.CmdArgs
import           System.Environment     (getArgs, withArgs)

import           Shell
import           SmartOS
import           VirtualBox

import qualified Prelude          as P
import           Prelude          hiding (FilePath)
import           Shelly

default (Text)

-----------------------------------------------------------------------------

data Option = Download
            | VBox
            deriving (Data, Eq, Show, Typeable)

main :: IO ()
main = do
  arguments  <- getArgs
  options    <- (if null arguments then withArgs ["--help"] else id)
                $ cmdArgsRun modes'
  verbosity' <- getVerbosity
  shelly
    $ case verbosity' of
      Quiet  -> silently  . print_stdout False . print_commands False
      Normal -> sub       . print_stdout False . print_commands False
      Loud   -> verbosely . print_stdout True  . print_commands True
    $ do platform' <- platform
         dispatch options platform'
  where
    modes' =
      cmdArgsMode $ modes modes''
      &= program (txtToLowerStr $ strToTxt name')
      &= help "Manage Your SmartOS Boxes."
      &= helpArg [ explicit, name "help", name "h" ]
      &= summary ( name' ++ " version " ++ version'
                   ++ " (c) 2012 Positive Inertia, LLC." )
      &= versionArg [ explicit, name "version", name "V", summary version' ]
      &= verbosity
    modes'' = [ Download &= help "Download the latest SmartOS Platform"
              , VBox     &= help "Setup a SmartOS VBox instance" ]
    name'    = "SmartBox"
    version' = "0.1.0"
    dispatch Download = checksumDownload
    dispatch VBox     = setupVBoxVM

-----------------------------------------------------------------------------

-- TODO need robust dependency-tracking
-- TODO need ssh/scp abilities for bootstrapping
-- TODO still need subcommands

data SmartBox = SmartBox { sbVm         :: VBoxVM
                         , sbVmDiskPath :: FilePath
                         , sbIsoPath    :: FilePath
                         , sbPlatform   :: SmartOS }
              deriving (Data, Show, Typeable, Eq)

setupVBoxVM :: SmartOS -> ShIO ()
setupVBoxVM pform@SmartOS{..} = do
  checksumDownload pform
  props <- vbSysProps
  home <- homePath
  let diskPath    = vmDirPath' </> "zones.vdi"
      isoPath'    = isoPath pform (isoDirPath home)
      vmBasePath  = findWithDefault "vm" "Default machine folder" props
      vmDirPath'  = vmBasePath </> (txtToStr . vmIdent $ vm)
      vm          = VBoxVM { vmIdent = "smartbox"
                           , vmDirPath = vmDirPath' }
  createOrUpdateVBoxVM SmartBox { sbVm         = vm
                                , sbVmDiskPath = diskPath
                                , sbIsoPath    = isoPath'
                                , sbPlatform   = pform }

createOrUpdateVBoxVM :: SmartBox -> ShIO ()
createOrUpdateVBoxVM sb@SmartBox{..} =
  do vbManageVM_ sbVm ShowVMInfo []
     updateVBoxVM sb
  `catch_sh`
  (\(_e :: SomeException) -> createVBoxVM sb)

createVBoxVM :: SmartBox -> ShIO ()
createVBoxVM sb@SmartBox{..} = do
  status ["Creating a SmartOS VBox instance", vmDirPath']
    $ vbManage_ CreateVM [ "--name", vmIdent sbVm
                         , "--ostype", "OpenSolaris_64"
                         , "--register" ]
  status ["Setting SmartOS VBox CPUs to 2 and Memory to 4096MB"]
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
  updateVBoxVM sb
  where vm         = vbManageVM_            $ sbVm
        vmDirPath' = fpToTxt . vmDirPath    $ sbVm
        vmDiskPath = fpToTxt                $ sbVmDiskPath

updateVBoxVM :: SmartBox -> ShIO ()
updateVBoxVM SmartBox{..} = do
  status ["Attaching SmartOS ISO", fpToTxt sbIsoPath]
    $ vbManageVM_ sbVm StorageAttach [ "--device", "0"
                                     , "--medium", toTextIgnore sbIsoPath
                                     , "--port", "1"
                                     , "--storagectl", "IDE Controller"
                                     , "--type", "dvddrive" ]
