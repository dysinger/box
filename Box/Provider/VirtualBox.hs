module Box.Provider.VirtualBox where

import           Box.Types
import qualified Data.ByteString.Char8  as DBC
import           Data.Char
import           Data.Map               (Map, fromList)
import           HSH

main :: IO ()
main = do
  -- TODO use cmd args to parse options only for virtualbox
  return ()

-----------------------------------------------------------------------------

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
        dropSpace = reverse . dropWhile isSpace

-----------------------------------------------------------------------------

createVm :: VM a => a -> IO ()
createVm s  = do
  runIO $ "VBoxManage createvm"
    ++ " --name " ++ (vmName s)
    ++ " --ostype OpenSolaris_64"
    ++ " --register"

createIde :: IO ()
createIde = do
  runIO $ "VBoxManage storagectl smartos"
    ++ " --add ide"
    ++ " --name 'IDE Controller'"

createDisk :: VM a => a -> IO ()
createDisk s = do
  runIO $ "VBoxManage createhd"
    ++ " --filename '" ++ (diskPath s) ++ "'"
    ++ " --size 40960"

attachISO :: VM a => a -> IO ()
attachISO s = do
  runIO $ "VBoxManage storageattach smartos"
    ++ " --device 0"
    ++ " --medium '" ++ (isoPath s) ++ "'"
    ++ " --port 1"
    ++ " --storagectl 'IDE Controller'"
    ++ " --type dvddrive"

attachDisk :: VM a => a -> IO ()
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

updateNetwork :: VM a => a -> IO ()
updateNetwork s = do
  runIO $ "VBoxManage modifyvm smartos"
    ++ " --bridgeadapter1 " ++ (interface s)
    ++ " --nic1 bridged"

startVm :: IO ()
startVm = runIO "VirtualBox --startvm smartos"

startVmHeadless :: IO ()
startVmHeadless = runIO "VBoxHeadless --startvm smartos"
