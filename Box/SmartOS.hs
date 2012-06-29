module Box.SmartOS
       ( SmartOS(..)
       , download
       , isoDirPath
       , isoPath
       , isoUrl
       , platform
       ) where

import           Box.Shell
import           Box.Text
import           Box.Types
import qualified Crypto.Conduit       as C
import           Data.Conduit         (($$))
import qualified Data.Conduit         as C
import qualified Data.Conduit.Binary  as C
import           Data.Digest.Pure.MD5 (MD5Digest)
import           Data.Text.Lazy       (Text)
import qualified Data.Text.Lazy       as T
import qualified Network.HTTP.Conduit as C
import           Prelude              hiding (FilePath)
import           Shelly
import           System.Directory     (doesFileExist)

default (Text)

-- TODO we need to download the md5sum as a separate cached file

isoUrl :: SmartOS -> Text
isoUrl SmartOS{..} = mirror isoName

isoPath :: SmartOS -> FilePath -> FilePath
isoPath SmartOS{..} = flip (</>) isoName

isoDirPath :: FilePath -> FilePath
isoDirPath = flip (</>) (".smartos" :: FilePath)

platform :: ShIO SmartOS
platform =
  status ["Fetching SmartOS Platform release list"] $ do
    bs <- liftIO . C.simpleHttp . txtToStr . mirror $ "md5sums.txt"
    let isIso  = filter . T.isInfixOf $ "iso"
        line   = last . (map T.words) . isIso . bsToTxtLines $ bs
    return SmartOS { isoMd5 = (line !! 0), isoName = (line !! 1) }

download :: SmartOS -> ShIO ()
download so@SmartOS{..} = do
  home <- homePath
  let path' = (isoDirPath home) </> isoName
      loop  = download' so >> download so
  isoExists <- liftIO $ doesFileExist (T.unpack $ toTextIgnore path')
  if isoExists
    then do c <- checksum so
            case c of
              Right _ -> return ()
              _       -> loop
    else loop

checksum :: SmartOS -> ShIO (Either Text Text)
checksum SmartOS{..} = do
  home <- homePath
  let isoPath' = toTextIgnore $ (isoDirPath home) </> isoName
  status ["Checking", isoPath'] $ do
    hash <- C.runResourceT $ C.sourceFile (txtToStr isoPath') $$ C.sinkHash
    let md5' = toHexTxt (hash :: MD5Digest)
    return $ if isoMd5 /= md5' then Left md5' else Right isoMd5

download' :: SmartOS -> ShIO ()
download' so@SmartOS{..} = do
  home <- homePath
  let isoDirPath' = isoDirPath $ home
      isoPath'    = isoDirPath' </> isoName
      isoUrl'     = isoUrl so
  status ["Downloading", fpToTxt isoPath'] $ do
    mkdir_p isoDirPath'
    request <- C.parseUrl (T.unpack isoUrl')
    C.withManager $ \manager -> do
      C.Response _ _ _ bsrc <- C.http request manager
      bsrc $$ C.sinkFile (fpToGfp isoPath')

mirror :: Text -> Text
mirror = T.append "https://download.joyent.com/pub/iso/"
