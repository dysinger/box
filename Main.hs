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
            | Setup
            deriving (Data, Eq, Show, Typeable)

main :: IO ()
main = do
  xs    <- getArgs
  opts  <- (if null xs then withArgs ["--help"] else id) $ cmdArgsRun modes'
  home  <- getHomeDirectory
  pform <- platform
  putStrLn $ "Latest SmartOS Platform is " ++ isoName pform
  dispatch opts pform home
  where
    modes' =
      cmdArgsMode $ modes modes''
      &= help "Manage Your SmartOS Boxes."
      &= helpArg [ explicit, name "help", name "h" ]
      &= program (DTL.unpack $ DTL.toLower $ DTL.pack name')
      &= summary ( summary' ++ " (c) 2012 Positive Inertia, LLC." )
      &= verbosityArgs [ explicit, name "Verbose", name "V" ] []
      &= versionArg [ explicit, name "version", name "v", summary summary' ]
    modes'' = [ Download &= help "Download the latest SmartOS Platform"
              , Setup    &= help "Setup a SmartOS VirtualBox instance" ]
    name'    = "SmartBox"
    version' = "0.1.0"
    summary' = name' ++ " version " ++ version'
    dispatch Download = download
    dispatch Setup    = setup

-----------------------------------------------------------------------------

-- TODO refactor more - need dependency-tracking tasks - checkout shake pkg

download :: SmartOS -> FilePath -> IO ()
download pform home = do
  let url'  = isoUrl pform
      dir'  = isoDirPath home
      path' = isoPath pform dir'
  createDirectoryIfMissing True dir'
  exists <- doesFileExist path'
  if exists
    then do putStr $ "Checking " ++ path' ++ " "
            c <- checkIso path' $ isoMd5 pform
            case c of
              Right _ -> do putStrLn "[GOOD]"
                            return ()
              _       -> do putStrLn "[CORRUPT!]"
                            download' url' path'
    else download' url' path'
  where download' u p = do
          putStrLn $ "Downloading " ++ isoName pform
          downloadIso u p
          download pform home

data SmartBox = SmartBox { sbVm         :: VBoxVM
                         , sbVmDirPath  :: FilePath
                         , sbVmDiskPath :: FilePath
                         , sbIsoPath    :: FilePath
                         , sbPlatform   :: SmartOS
                         }
              deriving (Data, Show, Typeable, Eq)

setup :: SmartOS -> FilePath -> IO ()
setup pform home = do
  exists <- doesFileExist $ isoPath pform $ isoDirPath home
  unless exists $ download pform home
  shelly $ silently $ do
    properties <- vbSysProps
    let vm     = VBoxVM "smartbox"
        folder = findWithDefault "vm" "Default machine folder" $ properties
        dir    = combine folder . DTL.unpack . ident           $ vm
        disk   = flip combine "zones.vdi"                      $ dir
        isoDir = isoDirPath home
        iso    = isoPath pform isoDir
    createOrUpdate SmartBox { sbVm         = vm
                            , sbVmDirPath  = dir
                            , sbVmDiskPath = disk
                            , sbIsoPath    = iso
                            , sbPlatform   = pform
                            }
    echo "Done"

createOrUpdate :: SmartBox -> ShIO ()
createOrUpdate sb@SmartBox{..} =
  do vbManageVM_ sbVm ShowVMInfo []
     update sb
  `catch_sh`
  (\(_e :: SomeException) -> create sb)

create :: SmartBox -> ShIO ()
create SmartBox{..} = do
  echo "Creating VirtualBox instance"
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

update :: SmartBox -> ShIO ()
update SmartBox{..} = do
  echo "Updating VirtualBox instance"
  v StorageAttach
    [ "--device", "0"
    , "--medium", DTL.pack sbIsoPath
    , "--port", "1"
    , "--storagectl", "IDE Controller"
    , "--type", "dvddrive"
    ]
  where v = vbManageVM_ sbVm
