module Box.SmartBox
       ( mode
       , main
       ) where

import           Box.SmartOS
import           Box.VirtualBox
import           Control.Exception               (SomeException)
import           Data.Data
import           Data.Map                        (findWithDefault)
import qualified Data.Text.Lazy                  as DTL
import           Shelly                          hiding (FilePath)
import qualified System.Console.CmdArgs.Explicit as SCCE
import           System.Console.CmdArgs.Explicit hiding (mode)
import           System.Directory
import           System.FilePath.Posix

default (DTL.Text)

-----------------------------------------------------------------------------

-- TODO add some cmdargs subcommands (create, update, delete) with
-- their own flags

mode :: Mode [(String,String)]
mode = modes "smartos" [] "SmartOS Management" [ createMode ]

createMode :: Mode [(String,String)]
createMode = SCCE.mode "create" [] "create a smartos instance"
             (flagArg (upd "file") "FILE") []
  where upd msg x v = Right $ (msg,x):v

-----------------------------------------------------------------------------

data SmartBox = SmartBox { sbVm         :: VBoxVM
                         , sbVmDirPath  :: FilePath
                         , sbVmDiskPath :: FilePath
                         , sbIsoPath    :: FilePath
                         , sbPlatform   :: SmartOS
                         }
              deriving (Data, Show, Typeable, Eq)

-----------------------------------------------------------------------------

main :: IO ()
main = do
  home     <- getHomeDirectory
  platform <- soPlatform
  shelly $ do
    properties <- vbSysProps
    let vm     = VBoxVM "smartbox"
        folder = findWithDefault "vm" "Default machine folder" $ properties
        dir    = combine folder . DTL.unpack . ident           $ vm
        disk   = flip combine "zones.vdi"                      $ dir
        isoDir = soIsoDirPath home
        iso    = soIsoPath platform isoDir
    liftIO $ soDownload platform isoDir
    let sb = SmartBox { sbVm         = vm
                      , sbVmDirPath  = dir
                      , sbVmDiskPath = disk
                      , sbIsoPath  = iso
                      , sbPlatform = platform
                      }
    liftIO $ putStrLn $ show sb
    createOrUpdate sb

createOrUpdate :: SmartBox -> ShIO ()
createOrUpdate sb@SmartBox{..} =
  do vbManageVM_ sbVm ShowVMInfo []
     update sb
  `catch_sh`
  (\(_e :: SomeException) ->
    create sb)

create :: SmartBox -> ShIO ()
create SmartBox{..} = do
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
  v StorageAttach
    [ "--device", "0"
    , "--medium", DTL.pack sbIsoPath
    , "--port", "1"
    , "--storagectl", "IDE Controller"
    , "--type", "dvddrive"
    ]
  where v = vbManageVM_ sbVm
