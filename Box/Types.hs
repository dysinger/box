{-# LANGUAGE DeriveDataTypeable #-}

module Box.Types where

import Data.Map
import System.Console.CmdArgs
import System.FilePath.Posix

-----------------------------------------------------------------------------

data Command = VBox { sync :: Bool }
             | Joyent
             deriving (Data, Typeable, Show, Eq)

-----------------------------------------------------------------------------

class VM a where
  vmName    :: a -> String
  diskPath  :: a -> FilePath
  interface :: a -> String
  isoMd5    :: a -> String
  isoPath   :: a -> FilePath
  isoUrl    :: a -> String
  path      :: a -> FilePath
  path'     :: a -> FilePath -> FilePath

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
  diskPath s   = path' s $ smartos_name s ++ ".vdi"
  interface _s = "wlan0"
  isoMd5       = iso_md5 . smartos_iso
  isoPath s    = (path' s . iso_path . smartos_iso) s
  isoUrl       = url . iso_path . smartos_iso
  path s       =
    combine
    (findWithDefault "~/VirtualBox VMs" "Default machine folder" $ smartos_props s)
    $ smartos_name s
  path' s      = combine (path s)

url :: String -> String
url = (++) "https://download.joyent.com/pub/iso/"
