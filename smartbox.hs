module Main where

import qualified Crypto.Conduit          as CC
import qualified Data.ByteString.Base16  as DBB
import qualified Data.ByteString.Char8   as DBC
import qualified Data.Char               as DCh
import qualified Data.Conduit            as DCo
import qualified Data.Conduit.Binary     as DCB
import qualified Data.Digest.Pure.MD5    as DDPM
import qualified Data.Map                as DM
import qualified Data.Serialize          as DSe
import qualified Data.String             as DSt
import qualified Data.Text               as DT
import qualified Data.Text.Lazy          as DTL
import qualified Data.Text.Lazy.Encoding as DTLE
import qualified HSH
import qualified Network.HTTP.Conduit    as NHC
import qualified System.Directory        as SD
import qualified System.FilePath.Posix   as SFP

main :: IO ()
main = do
  p <- properties
  r <- updateRelease
  let s = SmartOS { smartos_name  = "smartos"
                  , smartos_iso   = r
                  , smartos_props = p }
  downloadISO s
  createOrUpdate s

data SmartOS = SmartOS { smartos_name  :: String
                       , smartos_iso   :: ISO
                       , smartos_props :: DM.Map String String
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
    SFP.combine
    (DM.findWithDefault "~/VirtualBox VMs" "Default machine folder" $ smartos_props s)
    $ smartos_name s
  path' s      = SFP.combine (path s)

url :: String -> String
url = (++) "https://download.joyent.com/pub/iso/"

properties :: IO (DM.Map String String)
properties = do
  p <- HSH.run "VBoxManage list systemproperties" :: IO String
  return $ DM.fromList $ map props $ lines p
  where props     = tuple . map clean . split . pack
        tuple     = (\(k:v:_) -> (k, v))
        clean     = trim . unpack
        lines     = DSt.lines
        split     = DBC.split ':'
        pack      = DBC.pack
        unpack    = DBC.unpack
        trim      = dropSpace . dropSpace
        dropSpace = reverse . dropWhile DCh.isSpace

updateRelease :: IO ISO
updateRelease = do
  bs <- NHC.simpleHttp $ url "md5sums.txt"
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
  SD.createDirectoryIfMissing True $ path s
  exists <- SD.doesFileExist $ isoPath s
  if exists then checksum else download
  where
    checksum = do
      hash <- DCo.runResourceT $ DCB.sourceFile (isoPath s) DCo.$$ CC.sinkHash
      let checksum = DBC.unpack $ DBB.encode $ DSe.encode (hash :: DDPM.MD5Digest)
      if checksum /= (isoMd5 s) then download else return ()
    download = do
      request <- NHC.parseUrl (isoUrl s)
      NHC.withManager $ \manager -> do
        NHC.Response _ _ _ bsrc <- NHC.http request manager
        bsrc DCo.$$ DCB.sinkFile $ isoPath s
      downloadISO s

createOrUpdate :: SmartOS -> IO ()
createOrUpdate s = do
  exists <- HSH.run $ "VBoxManage showvminfo smartos" :: IO Bool
  if exists /= True then create s else update s

create :: SmartOS -> IO ()
create s = do
  createVm s
  createIde
  createDisk s
  update s

createVm :: SmartOS -> IO ()
createVm s  = do
  HSH.runIO $ "VBoxManage createvm"
    ++ " --name " ++ (smartos_name s)
    ++ " --ostype OpenSolaris_64"
    ++ " --register"

createIde :: IO ()
createIde = do
  HSH.runIO $ "VBoxManage storagectl smartos"
    ++ " --add ide"
    ++ " --name 'IDE Controller'"

createDisk :: SmartOS -> IO ()
createDisk s = do
  HSH.runIO $ "VBoxManage createhd"
    ++ " --filename '" ++ (diskPath s) ++ "'"
    ++ " --size 40960"

update :: SmartOS -> IO ()
update s = do
  attachISO s
  attachDisk s
  updateCpuMem
  updateBootOrder
  updateNetwork s

attachISO :: SmartOS -> IO ()
attachISO s = do
  HSH.runIO $ "VBoxManage storageattach smartos"
    ++ " --device 0"
    ++ " --medium '" ++ (isoPath s) ++ "'"
    ++ " --port 1"
    ++ " --storagectl 'IDE Controller'"
    ++ " --type dvddrive"

attachDisk :: SmartOS -> IO ()
attachDisk s = do
  HSH.runIO $ "VBoxManage storageattach smartos"
    ++ " --device 0"
    ++ " --medium '" ++ (diskPath s) ++ "'"
    ++ " --port 0"
    ++ " --storagectl 'IDE Controller'"
    ++ " --type hdd"

updateCpuMem :: IO ()
updateCpuMem = do
  HSH.runIO $ "VBoxManage modifyvm smartos"
    ++ " --cpus 2"
    ++ " --memory 4096"

updateBootOrder :: IO ()
updateBootOrder = do
  HSH.runIO $ "VBoxManage modifyvm smartos"
    ++ " --boot1 dvd"
    ++ " --boot2 disk"
    ++ " --boot3 none"

updateNetwork :: SmartOS -> IO ()
updateNetwork s = do
  HSH.runIO $ "VBoxManage modifyvm smartos"
    ++ " --bridgeadapter1 " ++ (interface s)
    ++ " --nic1 bridged"

startVm :: IO ()
startVm = HSH.runIO "VirtualBox --startvm smartos"

startVmHeadless :: IO ()
startVmHeadless = HSH.runIO "VBoxHeadless --startvm smartos"
