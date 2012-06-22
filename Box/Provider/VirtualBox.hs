{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Box.Provider.VirtualBox where

import           Box.Types
import qualified Data.ByteString.Char8 as DBC
import           Data.Char
import           Data.Map              (Map, fromList)
import qualified Data.Text.Lazy        as DTL
import           Prelude               hiding (FilePath)
import           Shelly

default (DTL.Text)

-----------------------------------------------------------------------------

properties :: ShIO (Map String String)
properties = do
  p <- run "VBoxManage" [ "list", "systemproperties" ]
  return $ fromList $ map props $ lines $ DTL.unpack p
  where props     = tuple . map clean . split . pack
        tuple     = (\(k:v:_) -> (k, v))
        clean     = trim . unpack
        split     = DBC.split ':'
        pack      = DBC.pack
        unpack    = DBC.unpack
        trim      = dropSpace . dropSpace
        dropSpace = reverse . dropWhile isSpace

-----------------------------------------------------------------------------

-- TODO move these smartos specific functions to SmartBox

createVm :: VM a => a -> ShIO ()
createVm s  = createvm
              [ "--name", DTL.pack (vmName s)
              , "--ostype", "OpenSolaris_64"
              , "--register"
              ]

createIde :: ShIO ()
createIde = storagectl "smartos"
            [ "--add" , "ide"
            , "--name", "'IDE Controller'"
            ]

createDisk :: VM a => a -> ShIO ()
createDisk s = createhd
               [ "--filename", (DTL.pack $ "'" ++ diskPath s ++ "'")
               , "--size", "40960"
               ]

attachISO :: VM a => a -> ShIO ()
attachISO s = storageattach "smartos"
              [ "--device", "0"
              , "--medium ", DTL.pack $ "'" ++ (isoPath s) ++ "'"
              , "--port", "1"
              , "--storagectl", "'IDE Controller'"
              , "--type", "dvddrive"
              ]

attachDisk :: VM a => a -> ShIO ()
attachDisk s = storageattach "smartos"
               [ "--device", "0"
               , "--medium", DTL.pack $ "'" ++ (diskPath s) ++ "'"
               , "--port", "0"
               , "--storagectl" , "'IDE Controller'"
               , "--type", "hdd"
               ]

updateCpuMem :: ShIO ()
updateCpuMem = modifyvm "smartos"
               [ "--cpus", "2"
               , "--memory", "4096"
               ]

updateBootOrder :: ShIO ()
updateBootOrder = modifyvm "smartos"
                  [ "--boot1", "dvd"
                  , "--boot2", "disk"
                  , "--boot3", "none"
                  ]

updateNetwork :: VM a => a -> ShIO ()
updateNetwork s = modifyvm "smartos"
                  [ "--bridgeadapter1", DTL.pack (interface s)
                  , "--nic1", "bridged"
                  ]

-----------------------------------------------------------------------------

virtualbox :: Bool -> [DTL.Text] -> ShIO ()
virtualbox _headless@True  = run_ "VBoxHeadless"
virtualbox _headless@False = run_ "VirtualBox"

storagectl :: DTL.Text -> [DTL.Text] -> ShIO ()
storagectl = vBoxManageVmCmd_ "storagectl"

storageattach :: DTL.Text -> [DTL.Text] -> ShIO ()
storageattach = vBoxManageVmCmd_ "storageattach"

createhd :: [DTL.Text] -> ShIO ()
createhd = vBoxManageCmd_ "createhd"

createvm :: [DTL.Text] -> ShIO ()
createvm = vBoxManageCmd_ "createvm"

modifyvm :: DTL.Text -> [DTL.Text] -> ShIO ()
modifyvm = vBoxManageVmCmd_ "modifyvm"

-----------------------------------------------------------------------------

vBoxManage :: [DTL.Text] -> ShIO DTL.Text
vBoxManage = run "VBoxManage"

vBoxManage_ :: [DTL.Text] -> ShIO ()
vBoxManage_ = run_ "VBoxManage"

vBoxManageCmd_ :: DTL.Text -> [DTL.Text] -> ShIO ()
vBoxManageCmd_ command args = vBoxManage_ (command:args)

vBoxManageVmCmd_ :: DTL.Text -> DTL.Text -> [DTL.Text] -> ShIO ()
vBoxManageVmCmd_ command vmName args = vBoxManage_ (command:vmName:args)

-----------------------------------------------------------------------------
