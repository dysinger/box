{-# LANGUAGE RecordWildCards #-}

module Box.SmartBox where

import           Box.Types
import           Box.Provider.VirtualBox

import           Crypto.Conduit
import qualified Data.ByteString         as DB
import qualified Data.ByteString.Base16  as DBB
import qualified Data.ByteString.Char8   as DBC
import qualified Data.ByteString.Lazy    as DBL
import           Data.Conduit
import           Data.Conduit.Binary     hiding (dropWhile, lines)
import           Data.Digest.Pure.MD5
import qualified Data.Serialize          as DS
import           HSH
import           Network.HTTP.Conduit    hiding (def, path)
import           System.Directory

dispatch :: Command -> IO ()
dispatch vbox@VBox{..} = do
  putStrLn $ show sync
  p <- properties
  i <- release
  let s = SmartOS { smartos_name  = "smartos"
                  , smartos_iso   = i
                  , smartos_props = p }
  downloadISO s
  createOrUpdate s
dispatch _ = return ()

release :: IO ISO
release = do
  bs <- simpleHttp $ url "md5sums.txt"
  let iso    = filter (DBC.isInfixOf $ DBC.pack "iso")
      string = DBC.unpack
      words  = DBC.words
      strict = DB.concat . DBL.toChunks
      lines' = DBC.lines
      latest = map string . words . last . iso . lines' . strict $ bs
  return $ ISO { iso_md5 = latest !! 0, iso_path = latest !! 1 }

downloadISO :: SmartOS -> IO ()
downloadISO s = do
  createDirectoryIfMissing True $ path s
  exists <- doesFileExist $ isoPath s
  if exists then checksum else download
  where
    checksum = do
      hash <- runResourceT $ sourceFile (isoPath s) $$ sinkHash
      let checksum = DBC.unpack $ DBB.encode $ DS.encode (hash :: MD5Digest)
      if checksum /= (isoMd5 s) then download else return ()
    download = do
      request <- parseUrl (isoUrl s)
      withManager $ \manager -> do
        Response _ _ _ bsrc <- http request manager
        bsrc $$ sinkFile $ isoPath s
      downloadISO s

createOrUpdate :: SmartOS -> IO ()
createOrUpdate s = do
  exists <- run $ "VBoxManage showvminfo smartos" :: IO Bool
  if exists /= True then create s else update s
  where create s = do createVm s
                      createIde
                      createDisk s
                      update s
        update s = do attachISO s
                      attachDisk s
                      updateCpuMem
                      updateBootOrder
                      updateNetwork s
