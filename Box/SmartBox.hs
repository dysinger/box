module Box.SmartBox where

import           Box.VirtualBox
import           Crypto.Conduit
import qualified Data.ByteString                 as DB
import qualified Data.ByteString.Base16          as DBB
import qualified Data.ByteString.Char8           as DBC
import qualified Data.ByteString.Lazy            as DBL
import           Data.Conduit
import           Data.Conduit.Binary             hiding (dropWhile, lines)
import           Data.Digest.Pure.MD5
import           Data.Map                        (Map, findWithDefault)
import qualified Data.Serialize                  as DS
import qualified Data.Text.Lazy                  as DTL
import           Network.HTTP.Conduit            hiding (def, path)
import           Shelly                          hiding (FilePath)
import qualified System.Console.CmdArgs.Explicit as SCCE
import           System.Console.CmdArgs.Explicit hiding (mode)
import           System.Directory
import           System.FilePath.Posix

-----------------------------------------------------------------------------

class VM a where
  vmName    :: a -> String
  diskPath  :: a -> FilePath
  interface :: a -> String
  isoMd5    :: a -> String
  isoPath   :: a -> FilePath
  isoUrl    :: a -> String
  home      :: a -> FilePath
  file      :: a -> FilePath -> FilePath

-----------------------------------------------------------------------------

data SmartOS = SmartOS { smartos_name  :: String
                       , smartos_iso   :: ISO
                       , smartos_props :: Map String String
                       }
             deriving Show

data ISO = ISO { iso_path :: FilePath
               , iso_md5  :: String }
         deriving Show

instance VM SmartOS where
  vmName       = smartos_name
  diskPath s   = file s $ smartos_name s ++ ".vdi"
  interface _s = "wlan0"
  isoMd5       = iso_md5 . smartos_iso
  isoPath s    = (file s . iso_path . smartos_iso) s
  isoUrl       = url . iso_path . smartos_iso
  home s       =
    combine
    (findWithDefault "~/VirtualBox VMs" "Default machine folder" $ smartos_props s)
    $ smartos_name s
  file s       = combine (home s)

url :: String -> String
url = (++) "https://download.joyent.com/pub/iso/"

-----------------------------------------------------------------------------

-- TODO add some subcommands (create, update, delete) with their own flags
-- TODO get a nice looking help working

mode :: Mode [(String,String)]
mode = modes "smartos" [] "SmartOS Management" [ createMode
                                               , deleteMode ]

createMode :: Mode [(String,String)]
createMode = SCCE.mode "create" [] "create a smartos instance"
             (flagArg (upd "file") "FILE") []
  where upd msg x v = Right $ (msg,x):v

deleteMode :: Mode [(String,String)]
deleteMode = SCCE.mode "delete" [] "delete a smartos instance"
             (flagArg (upd "file") "FILE") []
  where upd msg x v = Right $ (msg,x):v

-----------------------------------------------------------------------------

dispatch :: String -> ShIO ()
dispatch _ = do
  props <- properties
  iso <- liftIO platformVersion
  let release = SmartOS { smartos_name  = "smartos"
                        , smartos_iso   = iso
                        , smartos_props = props }
  liftIO $ downloadPlatform release
  createOrUpdate release (VBoxVM (DTL.pack $ vmName release))

-----------------------------------------------------------------------------

platformVersion :: IO ISO
platformVersion = do
  bs <- simpleHttp $ url "md5sums.txt"
  let iso    = filter (DBC.isInfixOf $ DBC.pack "iso")
      string = DBC.unpack
      words' = DBC.words
      strict = DB.concat . DBL.toChunks
      lines' = DBC.lines
      latest = map string . words' . last . iso . lines' . strict $ bs
  return $ ISO { iso_md5 = latest !! 0, iso_path = latest !! 1 }

downloadPlatform :: SmartOS -> IO ()
downloadPlatform s = do
  createDirectoryIfMissing True $ home s
  exists <- doesFileExist $ isoPath s
  if exists then checksum else download
  where
    checksum = do
      hash <- runResourceT $ sourceFile (isoPath s) $$ sinkHash
      let checksum' = DBC.unpack $ DBB.encode $ DS.encode (hash :: MD5Digest)
      if checksum' /= (isoMd5 s) then download else return ()
    download = do
      request <- parseUrl (isoUrl s)
      withManager $ \manager -> do
        Response _ _ _ bsrc <- http request manager
        bsrc $$ sinkFile $ isoPath s
      downloadPlatform s

-----------------------------------------------------------------------------

createOrUpdate :: SmartOS -> VBoxVM -> ShIO ()
createOrUpdate release vm = do
  manageVM vm ShowInfo []
  errs <- lastStderr
  let manager = manageVM_ vm
    in if errs == ""
       then create manager release
       else update manager release

create :: (VBoxManageVmCmd -> [DTL.Text] -> ShIO ()) -> SmartOS -> ShIO ()
create vmMgr release = do
  manage_ CreateVM
    [ "--name", DTL.pack (vmName release)
    , "--ostype", "OpenSolaris_64"
    , "--register"
    ]
  manage_ CreateIDE
    [ "--add" , "ide"
    , "--name", "'IDE Controller'"
    ]
  manage_ CreateHD
    [ "--filename", (DTL.pack $ "'" ++ diskPath release ++ "'")
    , "--size", "40960"
    ]
  update vmMgr release

update :: (VBoxManageVmCmd -> [DTL.Text] -> ShIO ()) -> SmartOS -> ShIO ()
update vmMgr release = do
  vmMgr StorageAttach
    [ "--device", "0"
    , "--medium ", DTL.pack $ "'" ++ (isoPath release) ++ "'"
    , "--port", "1"
    , "--storagectl", "'IDE Controller'"
    , "--type", "dvddrive"
    ]
  vmMgr StorageAttach
    [ "--device", "0"
    , "--medium", DTL.pack $ "'" ++ (diskPath release) ++ "'"
    , "--port", "0"
    , "--storagectl" , "'IDE Controller'"
    , "--type", "hdd"
    ]
  vmMgr ModifyVM
    [ "--cpus", "2"
    , "--memory", "4096"
    ]
  vmMgr ModifyVM
    [ "--boot1", "dvd"
    , "--boot2", "disk"
    , "--boot3", "none"
    ]
  vmMgr ModifyVM
    [ "--bridgeadapter1", DTL.pack (interface release)
    , "--nic1", "bridged"
    ]
