module Box.SmartOS
       ( SmartOS(..)
       , isoDirPath
       , isoPath
       , isoUrl
       , platform
       , download
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
import           System.Directory     (getModificationTime, doesFileExist)
import           System.Time

default (Text)

isoUrl :: SmartOS -> Text
isoUrl SmartOS{..} = mirror isoName

isoPath :: SmartOS -> FilePath -> FilePath
isoPath SmartOS{..} = flip (</>) isoName

isoDirPath :: FilePath -> FilePath
isoDirPath = flip (</>) (".smartos" :: FilePath)

mirror :: Text -> Text
mirror = T.append "https://download.joyent.com/pub/iso/"

-----------------------------------------------------------------------------

platform :: ShIO SmartOS
platform = do
  home <- homePath
  let filePath = isoDirPath home </> ("md5sums.txt" :: FilePath)
  isUpToDate <- isMetadataUpToDate filePath
  if isUpToDate then return () else cacheMetadata filePath
  contents <- liftIO . readFile . fpToGfp $ filePath
  let isIso = filter . T.isInfixOf $ "iso"
      line  = last . (map T.words) . isIso . T.lines . strToTxt $ contents
  return SmartOS { isoMd5 = line !! 0, isoName = line !! 1 }

isMetadataUpToDate :: FilePath -> ShIO Bool
isMetadataUpToDate filePath = do
  let filePath' = fpToGfp filePath
  localTime <- liftIO getClockTime
  exists    <- liftIO . doesFileExist $ filePath'
  if exists
    then do fileTime <- liftIO . getModificationTime $ filePath'
            let diff = diffClockTimes localTime fileTime
            return $ (tdDay diff) + (tdMonth diff) + (tdYear diff) == 0
    else return False

cacheMetadata :: FilePath -> ShIO ()
cacheMetadata filePath =
  status ["Updating SmartOS Platform metadata"] $ do
    let url = mirror "md5sums.txt"
    request <- C.parseUrl . txtToStr $ url
    C.withManager $ \manager -> do
      C.Response _ _ _ bsrc <- C.http request manager
      bsrc $$ C.sinkFile . fpToGfp $ filePath

-----------------------------------------------------------------------------

download :: ShIO ()
download = do
  so@SmartOS{..} <- platform
  home <- homePath
  let filePath = (isoDirPath home) </> isoName
      loop     = downloadISO so home >> download
  isoExists <- liftIO $ doesFileExist . fpToGfp $ filePath
  if isoExists
    then do c <- checksum so home
            case c of
              Right _ -> return ()
              _       -> loop
    else loop

checksum :: SmartOS -> FilePath -> ShIO (Either Text Text)
checksum SmartOS{..} home = do
  let isoPath' = toTextIgnore $ (isoDirPath home) </> isoName
  status ["Checking", isoPath'] $ do
    hash <- C.runResourceT $ C.sourceFile (txtToStr isoPath') $$ C.sinkHash
    let md5' = toHexTxt (hash :: MD5Digest)
    return $ if isoMd5 /= md5' then Left md5' else Right isoMd5

downloadISO :: SmartOS -> FilePath -> ShIO ()
downloadISO so@SmartOS{..} home = do
  let isoDirPath' = isoDirPath $ home
      isoPath'    = isoDirPath' </> isoName
      isoUrl'     = isoUrl so
  status ["Downloading", fpToTxt isoPath'] $ do
    mkdir_p isoDirPath'
    request <- C.parseUrl (T.unpack isoUrl')
    C.withManager $ \manager -> do
      C.Response _ _ _ bsrc <- C.http request manager
      bsrc $$ C.sinkFile (fpToGfp isoPath')
