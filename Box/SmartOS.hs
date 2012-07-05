module Box.SmartOS
       ( SmartOS(..)
       , bootstrap
       , download
       , isoDirPath
       , isoPath
       , isoUrl
       , platform
       ) where

import           Box.Shelly
import           Box.Text
import           Box.Types

import qualified Crypto.Conduit       as C
import           Data.Conduit         (($$))
import qualified Data.Conduit         as C
import qualified Data.Conduit.Binary  as C
import           Data.Digest.Pure.MD5 (MD5Digest)
import qualified Data.Text.Lazy       as T
import qualified Network.HTTP.Conduit as C
import           System.Directory     (getModificationTime, doesFileExist)
import           System.Time

-- shelly imports
import           Prelude              hiding (FilePath)
import           Shelly               hiding (shelly)

-- default to Text strings
import           Data.Text.Lazy       (Text)
default (Text)

isoUrl :: SmartOS -> Text
isoUrl SmartOS{..} = mirror isoName

isoPath :: SmartOS -> FilePath -> FilePath
isoPath SmartOS{..} = flip (</>) isoName

isoDirPath :: FilePath -> FilePath
isoDirPath = flip (</>) ((".box" :: FilePath) </> ("smartos" :: FilePath))

mirror :: Text -> Text
mirror = T.append "https://download.joyent.com/pub/iso/"

createDir :: ShIO ()
createDir = homePath >>= mkdir_p . isoDirPath

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
    createDir
    request <- C.parseUrl . txtToStr . mirror $ "md5sums.txt"
    C.withManager $ \manager -> do
      C.Response _ _ _ bsrc <- C.http request manager
      bsrc $$ C.sinkFile . fpToGfp $ filePath

download :: ShIO ()
download = do
  so@SmartOS{..} <- platform
  home <- homePath
  let filePath = (isoDirPath home) </> isoName
      loop     = downloadISO so home >> download
  isoExists <- liftIO . doesFileExist . fpToGfp $ filePath
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
  let isoDirPath' = isoDirPath home
      isoPath'    = isoDirPath' </> isoName
      isoUrl'     = isoUrl so
  status ["Downloading", fpToTxt isoPath'] $ do
    createDir
    request <- C.parseUrl . txtToStr $ isoUrl'
    C.withManager $ \manager -> do
      C.Response _ _ _ bsrc <- C.http request manager
      bsrc $$ C.sinkFile (fpToGfp isoPath')

bootstrap :: ShIO ()
bootstrap = do
  return ()
