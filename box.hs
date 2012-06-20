{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

import           Crypto.Conduit
import qualified Data.ByteString.Base16  as DBB
import qualified Data.ByteString.Char8   as DBC
import qualified Data.Char               as DC
import           Data.Conduit
import           Data.Conduit.Binary     hiding (dropWhile, lines)
import qualified Data.Digest.Pure.MD5    as DDPM
import           Data.Map                (Map, findWithDefault, fromList)
import qualified Data.Serialize          as DS
import qualified Data.Text               as DT
import qualified Data.Text.Lazy          as DTL
import qualified Data.Text.Lazy.Encoding as DTLE
import           HSH
import           Network.HTTP.Conduit    hiding (def, path)
import           System.Console.CmdArgs
import           System.Directory
import           System.Environment
import           System.FilePath.Posix

-----------------------------------------------------------------------------

data Command = VBox { sync :: Bool }
             | Joyent
             deriving (Data, Typeable, Show, Eq)

main :: IO ()
main = do
  args <- getArgs
  opts <- (if null args then withArgs ["--help"] else id) $ cmdArgsRun modes'
  dispatch opts
  where
    modes'      = cmdArgsMode $ modes
                  [ VBox { sync = def &= help "Update SmartOS" }
                    &= details [ "Examples:"
                               , "box vbox --sync"
                               ]
                  , Joyent
                    &= details [ "Examples:"
                               , "box joyent"
                               ]
                  ]
                  &= program name'
                  &= help about'
                  &= summary (info' ++ ", " ++ copyright')
                  &= helpArg       [ explicit, name "help",    name "h" ]
                  &= verbosityArgs [ explicit, name "Verbose", name "V" ] []
                  &= versionArg    [ explicit
                                   , name "version"
                                   , name "v"
                                   , summary info' ]
    name'       = "box"
    version'    = "0.1.0.0"
    info'       = name' ++ " version " ++ version'
    about'      = "A command line application to manage boxes."
    copyright'  = "Copyright © 2012 Positive Inertia, LLC."

dispatch :: Command -> IO ()

-----------------------------------------------------------------------------

data SmartOS = SmartOS { smartos_name  :: String
                       , smartos_iso   :: ISO
                       , smartos_props :: Map String String
                       }
             deriving Show

data ISO = ISO { iso_path :: FilePath
               , iso_md5  :: String }
         deriving Show

class VM a where
  diskPath  :: a -> FilePath
  interface :: a -> String
  isoMd5    :: a -> String
  isoPath   :: a -> FilePath
  isoUrl    :: a -> String
  path      :: a -> FilePath
  path'     :: a -> FilePath -> FilePath

instance VM SmartOS where
  diskPath s   = path' s $ smartos_name s ++ ".vdi"
  interface _s = "wlan0"
  isoMd5 s     = iso_md5 $ smartos_iso s
  isoPath s    = (path' s . iso_path . smartos_iso) s
  isoUrl       = url . iso_path . smartos_iso
  path s       =
    combine
    (findWithDefault "~/VirtualBox VMs" "Default machine folder" $ smartos_props s)
    $ smartos_name s
  path' s      = combine (path s)

dispatch vbox@VBox{..} = do
  putStrLn $ show sync
  p <- properties
  r <- updateRelease
  let s = SmartOS { smartos_name  = "smartos"
                  , smartos_iso   = r
                  , smartos_props = p }
  downloadISO s
  createOrUpdate s

url :: String -> String
url = (++) "https://download.joyent.com/pub/iso/"

properties :: IO (Map String String)
properties = do
  p <- run "VBoxManage list systemproperties" :: IO String
  return $ fromList $ map props $ lines p
  where props     = tuple . map clean . split . pack
        tuple     = (\(k:v:_) -> (k, v))
        clean     = trim . unpack
        split     = DBC.split ':'
        pack      = DBC.pack
        unpack    = DBC.unpack
        trim      = dropSpace . dropSpace
        dropSpace = reverse . dropWhile DC.isSpace

updateRelease :: IO ISO
updateRelease = do
  bs <- simpleHttp $ url "md5sums.txt"
  let latest = map DT.unpack  -- TODO try using lazy char8 for this line/words biz
               . DT.words
               . last
               . filter (DT.isInfixOf (DT.pack "iso"))
               . DT.lines
               . DTL.toStrict
               $ DTLE.decodeUtf8 bs
  return $ ISO { iso_md5 = latest !! 0, iso_path = latest !! 1 }

downloadISO :: SmartOS -> IO ()
downloadISO s = do
  createDirectoryIfMissing True $ path s
  exists <- doesFileExist $ isoPath s
  if exists then checksum else download
  where
    checksum = do
      hash <- runResourceT $ sourceFile (isoPath s) $$ sinkHash
      let checksum = DBC.unpack $ DBB.encode $ DS.encode (hash :: DDPM.MD5Digest)
      if checksum /= (isoMd5 s) then download else return ()
    download = do
      request <- parseUrl (isoUrl s)
      withManager $ \manager -> do
        Response _ _ _ bsrc <- http request manager
        bsrc $$ sinkFile $ isoPath s
      downloadISO s

-----------------------------------------------------------------------------

createOrUpdate :: SmartOS -> IO ()
createOrUpdate s = do
  exists <- run $ "VBoxManage showvminfo smartos" :: IO Bool
  if exists /= True then create s else update s

create :: SmartOS -> IO ()
create s = do
  createVm s
  createIde
  createDisk s
  update s

update :: SmartOS -> IO ()
update s = do
  attachISO s
  attachDisk s
  updateCpuMem
  updateBootOrder
  updateNetwork s

-----------------------------------------------------------------------------

createVm :: SmartOS -> IO ()
createVm s  = do
  runIO $ "VBoxManage createvm"
    ++ " --name " ++ (smartos_name s)
    ++ " --ostype OpenSolaris_64"
    ++ " --register"

createIde :: IO ()
createIde = do
  runIO $ "VBoxManage storagectl smartos"
    ++ " --add ide"
    ++ " --name 'IDE Controller'"

createDisk :: SmartOS -> IO ()
createDisk s = do
  runIO $ "VBoxManage createhd"
    ++ " --filename '" ++ (diskPath s) ++ "'"
    ++ " --size 40960"

attachISO :: SmartOS -> IO ()
attachISO s = do
  runIO $ "VBoxManage storageattach smartos"
    ++ " --device 0"
    ++ " --medium '" ++ (isoPath s) ++ "'"
    ++ " --port 1"
    ++ " --storagectl 'IDE Controller'"
    ++ " --type dvddrive"

attachDisk :: SmartOS -> IO ()
attachDisk s = do
  runIO $ "VBoxManage storageattach smartos"
    ++ " --device 0"
    ++ " --medium '" ++ (diskPath s) ++ "'"
    ++ " --port 0"
    ++ " --storagectl 'IDE Controller'"
    ++ " --type hdd"

updateCpuMem :: IO ()
updateCpuMem = do
  runIO $ "VBoxManage modifyvm smartos"
    ++ " --cpus 2"
    ++ " --memory 4096"

updateBootOrder :: IO ()
updateBootOrder = do
  runIO $ "VBoxManage modifyvm smartos"
    ++ " --boot1 dvd"
    ++ " --boot2 disk"
    ++ " --boot3 none"

updateNetwork :: SmartOS -> IO ()
updateNetwork s = do
  runIO $ "VBoxManage modifyvm smartos"
    ++ " --bridgeadapter1 " ++ (interface s)
    ++ " --nic1 bridged"

startVm :: IO ()
startVm = runIO "VirtualBox --startvm smartos"

startVmHeadless :: IO ()
startVmHeadless = runIO "VBoxHeadless --startvm smartos"
