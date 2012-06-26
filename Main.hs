import           Control.Exception      (SomeException)
import           Data.Data
import           Data.Map               (findWithDefault)
import qualified Data.Text.Lazy         as DTL
import           Shelly                 hiding (FilePath)
import           System.Console.CmdArgs
import           System.Directory
import           System.Environment
import           System.FilePath.Posix

import           SmartOS
import           VirtualBox

default (DTL.Text)

-----------------------------------------------------------------------------
data Option = Download
            | VBox
            deriving (Data, Eq, Show, Typeable)

main :: IO ()
main = do
  xs    <- getArgs
  opts  <- (if null xs then withArgs ["--help"] else id) $ cmdArgsRun modes'
  home  <- getHomeDirectory
  verb  <- getVerbosity
  pform <- platform
  shelly
    $ case verb of
      Quiet  -> silently
      Normal -> sub
      Loud   -> verbosely
    $ dispatch opts pform home
  where
    modes' =
      cmdArgsMode $ modes modes''
      &= program (DTL.unpack $ DTL.toLower $ DTL.pack name')
      &= help "Manage Your SmartOS Boxes."
      &= helpArg [ explicit, name "help", name "h" ]
      &= summary ( name' ++ " version " ++ version'
                   ++ " (c) 2012 Positive Inertia, LLC." )
      &= versionArg [ explicit, name "version", name "V", summary version' ]
      &= verbosity
    modes'' = [ Download   &= help "Download the latest SmartOS Platform"
              , VBox &= help "Setup a SmartOS VBox instance" ]
    name'    = "SmartBox"
    version' = "0.1.0"
    dispatch Download   = downloadSmartOS
    dispatch VBox = setupVBoxVM

-----------------------------------------------------------------------------

-- TODO refactor more - need dependency-tracking tasks - checkout shake pkg

downloadSmartOS :: SmartOS -> FilePath -> ShIO ()
downloadSmartOS pform home = do
  let url'  = isoUrl pform
      dir'  = isoDirPath home
      path' = isoPath pform dir'
  liftIO $ createDirectoryIfMissing True dir'
  isoExists <- liftIO $ doesFileExist path'
  if isoExists
    then do echo_n $ DTL.pack $ "Checking " ++ path' ++ " "
            c <- liftIO $ checkIso path' $ isoMd5 pform
            case c of
              Right _ -> do echo "[GOOD]"
                            return ()
              _       -> do echo "[CORRUPT!]"
                            download' url' path'
    else download' url' path'
  where download' u p = do
          echo $ DTL.pack $ "Downloading " ++ (show $ isoName pform)
          liftIO $ downloadIso u p
          downloadSmartOS pform home

data SmartBox = SmartBox { sbVm         :: VBoxVM
                         , sbVmDirPath  :: FilePath
                         , sbVmDiskPath :: FilePath
                         , sbIsoPath    :: FilePath
                         , sbPlatform   :: SmartOS
                         }
              deriving (Data, Show, Typeable, Eq)

setupVBoxVM :: SmartOS -> FilePath -> ShIO ()
setupVBoxVM pform home = do
  isoExists <- liftIO $ doesFileExist $ isoPath pform $ isoDirPath home
  unless isoExists $ downloadSmartOS pform home
  shelly $ silently $ do
    properties <- vbSysProps
    let vm     = VBoxVM "smartbox"
        folder = findWithDefault "vm" "Default machine folder" $ properties
        dir    = combine folder . DTL.unpack . ident           $ vm
        disk   = flip combine "zones.vdi"                      $ dir
        isoDir = isoDirPath home
        iso    = isoPath pform isoDir
    createOrUpdateVBoxVM SmartBox { sbVm         = vm
                                  , sbVmDirPath  = dir
                                  , sbVmDiskPath = disk
                                  , sbIsoPath    = iso
                                  , sbPlatform   = pform
                                  }
    echo "Done"

createOrUpdateVBoxVM :: SmartBox -> ShIO ()
createOrUpdateVBoxVM sb@SmartBox{..} =
  do vbManageVM_ sbVm ShowVMInfo []
     updateVBoxVM sb
  `catch_sh`
  (\(_e :: SomeException) -> createVBoxVM sb)

createVBoxVM :: SmartBox -> ShIO ()
createVBoxVM SmartBox{..} = do
  echo "Creating SmartOS VBox instance"
  vbManage_ CreateVM
    [ "--name", ident sbVm
    , "--ostype", "OpenSolaris_64"
    , "--register"
    ]
  v ModifyVM [ "--cpus", "2", "--memory", "4096" ]
  v StorageCtl [ "--add" , "ide", "--name", "IDE Controller" ]
  vbManage_ CreateHD [ "--filename", DTL.pack sbVmDiskPath, "--size", "40960" ]
  v StorageAttach
    [ "--device", "0"
    , "--medium", DTL.pack sbVmDiskPath
    , "--port", "0"
    , "--storagectl" , "IDE Controller"
    , "--type", "hdd"
    ]
  v StorageAttach
    [ "--device", "0"
    , "--medium", DTL.pack sbIsoPath
    , "--port", "1"
    , "--storagectl", "IDE Controller"
    , "--type", "dvddrive"
    ]
  v ModifyVM [ "--boot1", "dvd", "--boot2", "disk", "--boot3", "none" ]
  v ModifyVM [ "--natpf1", "SSH,tcp,,2222,,22" ]
  v ModifyVM [ "--nic2", "bridged", "--bridgeadapter2", DTL.pack "wlan0" ]
  v ModifyVM [ "--nic3", "bridged", "--bridgeadapter3", DTL.pack "wlan0" ]
  v ModifyVM [ "--nic4", "bridged", "--bridgeadapter4", DTL.pack "wlan0" ]
  where v = vbManageVM_ sbVm

updateVBoxVM :: SmartBox -> ShIO ()
updateVBoxVM SmartBox{..} = do
  echo "Updating SmartOS VBox instance"
  v StorageAttach
    [ "--device", "0"
    , "--medium", DTL.pack sbIsoPath
    , "--port", "1"
    , "--storagectl", "IDE Controller"
    , "--type", "dvddrive"
    ]
  where v = vbManageVM_ sbVm
