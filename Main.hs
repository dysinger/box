module Main (main) where

import           Control.Exception
import           Data.Data
import           Data.Map               hiding (null)
import           Data.Text.Lazy         (Text)
import           Prelude                hiding (FilePath)
import           Shelly
import           Box.Shell
import           Box.SmartOS
import           Box.Text
import           Box.VBox
import           System.Console.CmdArgs
import           System.Environment

default (Text)

-----------------------------------------------------------------------------
-- CmdArgs Argument/Option Processing

-- TODO still need subcommands

data Cmd = Download
         | VBox
         | Bootstrap { host :: Maybe String
                     , port :: Maybe Int }
         deriving (Data, Eq, Show, Typeable)

mode :: Mode (CmdArgs Cmd)
mode =
  cmdArgsMode $ modes appCmds
  &= program (txtToLowerStr $ strToTxt name')
  &= help "Manage Your SmartOS Boxes."
  &= helpArg [ explicit, name "help", name "h" ]
  &= summary ( name' ++ " version " ++ version'
               ++ " (c) 2012 Positive Inertia, LLC." )
  &= versionArg [ explicit, name "version", name "V", summary version' ]
  &= verbosity
  where
    name'    = "Box"
    version' = "0.1.0"

appCmds :: [Cmd]
appCmds =
  [ Download
    &= help "Check & Download the latest SmartOS Platform"
  , VBox
    &= help "Setup or Update your SmartOS VBox instance"
  , Bootstrap { host = def
                       &= help "The host to bootstrap"
                       &= opt (Just ("127.0.0.1" :: String))
                       &= typ "HOST"
              , port = def
                       &= help "The ssh port to use"
                       &= opt (Just (22 :: Int))
                       &= typ "PORT"
              }
    &= help "Bootstrap a SmartOS Box as a VM host"
  ]

-- | Main entry point & processor of command line arguments
main :: IO ()
main = do
  arguments  <- getArgs
  options    <- (if null arguments then withArgs ["--help"] else id)
                $ cmdArgsRun mode
  verbosity' <- getVerbosity
  shelly
    $ case verbosity' of
      Quiet  -> silently  . print_stdout False . print_commands False
      Normal -> sub       . print_stdout False . print_commands False
      Loud   -> verbosely . print_stdout True  . print_commands True
    $ do platform' <- platform
         dispatch options platform'

-- | Dispatch a Cmd to it's appropriate function
dispatch :: Cmd -> SmartOS -> ShIO ()
dispatch Download        = checksumDownload
dispatch VBox            = setupVBoxVM
dispatch b@Bootstrap{..} = flip bootstrap b

-----------------------------------------------------------------------------
-- Possible Exceptions

data Ex = BootstrapEx Text
        deriving (Data, Eq, Show, Typeable)

instance Exception Ex

-----------------------------------------------------------------------------
-- VirtualBox Interaction

data Box = Box { sbVm         :: VBoxVM
               , sbVmDiskPath :: FilePath
               , sbIsoPath    :: FilePath
               , sbPlatform   :: SmartOS }
         deriving (Data, Show, Typeable, Eq)

-- TODO need robust run-once dependency-tracking between tasks

-- | Boot an existing SmartoS VBox instance
-- bootVBoxVM :: SmartOS -> ShIO ()
-- bootVBoxVM so@SmartOS{..} = do
--   -- deps
--   setupVBoxVM so
--   -- boot
--   -- boot virtualbox vm
--   -- TODO where do we get the VBox instance? needs extracting

-- | Setup a VirtualBox instance with 4GB ram & 40GB disk for SmartOS
setupVBoxVM :: SmartOS -> ShIO ()
setupVBoxVM so@SmartOS{..} = do
  -- deps
  checksumDownload so
  -- setup
  props <- vbSysProps
  home <- homePath
  let diskPath    = vmDirPath' </> ("zones.vdi" :: FilePath)
      isoPath'    = isoPath so (isoDirPath home)
      vmBasePath  = findWithDefault "vm" "Default machine folder" props
      vmDirPath'  = vmBasePath </> (txtToStr . vmIdent $ vm)
      vm          = VBoxVM { vmIdent = "smartbox"
                           , vmDirPath = vmDirPath' }
  createOrUpdateVBoxVM Box { sbVm         = vm
                           , sbVmDiskPath = diskPath
                           , sbIsoPath    = isoPath'
                           , sbPlatform   = so }

-- | create or update a VirtualBox instance depending on the current state
createOrUpdateVBoxVM :: Box -> ShIO ()
createOrUpdateVBoxVM sb@Box{..} =
  do vbManageVM_ sbVm ShowVMInfo []
     updateVBoxVM sb
  `catch_sh`
  (\(_e :: SomeException) -> createVBoxVM sb)

-- | create a new VirtualBox instance
createVBoxVM :: Box -> ShIO ()
createVBoxVM sb@Box{..} = do
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

-- | update an existing VirtualBox instance
updateVBoxVM :: Box -> ShIO ()
updateVBoxVM Box{..} = do
  status ["Attaching SmartOS ISO", fpToTxt sbIsoPath]
    $ vbManageVM_ sbVm StorageAttach [ "--device", "0"
                                     , "--medium", toTextIgnore sbIsoPath
                                     , "--port", "1"
                                     , "--storagectl", "IDE Controller"
                                     , "--type", "dvddrive" ]

-----------------------------------------------------------------------------
-- Bootstrap SmartOS

-- TODO need ssh/scp abilities for bootstrapping

-- | Bootstrap (initial setup) SmartOS Platform box over SSH
bootstrap :: SmartOS -> Cmd -> ShIO ()
bootstrap _so@SmartOS{..} _conf@Bootstrap{..} = do
  -- TODO where do we get the ssh info? needs args/opts
  -- take all our args
  -- & ssh into the box
  -- & setup ssh-conduit
  -- & login as "root/root"
  -- & run function to setup dsadm & do an update
  -- & print the output of run
  return ()
