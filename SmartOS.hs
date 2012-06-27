module SmartOS
       ( SmartOS(..)
       , checksum
       , checksumDownload
       , download
       , isoDirPath
       , isoPath
       , isoUrl
       , platform
       ) where

import           Crypto.Conduit
import qualified Data.ByteString.Base16     as DBB
import qualified Data.ByteString.Char8      as DBC
import           Data.Conduit
import           Data.Conduit.Binary        hiding (dropWhile, lines, take)
import           Data.Data
import           Data.Digest.Pure.MD5       hiding (md5)
import qualified Data.Serialize             as DS
import qualified Data.Text.Lazy             as DTL
import qualified Data.Text.Lazy.Encoding    as DTLE
import           Network.HTTP.Conduit       hiding (def, path)
import           Prelude                    hiding (FilePath)
import           Shell
import           System.Directory           (doesFileExist)

default (DTL.Text)

-----------------------------------------------------------------------------

data SmartOS = SmartOS { isoName :: DTL.Text
                       , isoMd5  :: DTL.Text }
              deriving (Data, Show, Typeable, Eq)

isoUrl :: SmartOS -> DTL.Text
isoUrl SmartOS{..} = mirror isoName

isoPath :: SmartOS -> FilePath -> FilePath
isoPath SmartOS{..} = flip (</>) isoName

isoDirPath :: FilePath -> FilePath
isoDirPath = flip (</>) (".smartos" :: FilePath)

platform :: ShIO SmartOS
platform =
  status "Fetching SmartOS Platform release list" $ do
    bs <- liftIO . simpleHttp . DTL.unpack . mirror $ "md5sums.txt"
    let words' = map DTL.words
        isos   = filter . DTL.isInfixOf $ "iso"
        last'  = last . words' . isos . DTL.lines . DTLE.decodeUtf8 $ bs
    return SmartOS { isoMd5 = (last' !! 0), isoName = (last' !! 1) }

checksumDownload :: SmartOS -> ShIO ()
checksumDownload pform@SmartOS{..} = do
  home <- homePath
  let path'     = (isoDirPath home) </> isoName
      download' = download pform >> checksumDownload pform
  isoExists <- liftIO $ doesFileExist (DTL.unpack $ toTextIgnore path')
  if isoExists
    then do c <- checksum pform
            case c of
              Right _ -> return ()
              _       -> download'
    else download'

checksum :: SmartOS -> ShIO (Either DTL.Text DTL.Text)
checksum SmartOS{..} = do
  home <- homePath
  let isoPath' = toTextIgnore $ (isoDirPath home) </> isoName
  status (DTL.append "Checking " isoPath') $ do
    hash <- runResourceT $ sourceFile (DTL.unpack isoPath') $$ sinkHash
    let md5' = DTL.pack . DBC.unpack . DBB.encode . DS.encode $ (hash :: MD5Digest)
    return $ if isoMd5 /= md5' then Left md5' else Right isoMd5

download :: SmartOS -> ShIO ()
download pform@SmartOS{..} = do
  home <- homePath
  let isoDirPath' = isoDirPath $ home
      isoPath'    = isoDirPath' </> isoName
      isoPath''   = toTextIgnore isoPath'
      isoUrl'     = isoUrl pform
  status (DTL.append "Downloading " isoPath'') $ do
    mkdir_p isoDirPath'
    request <- parseUrl (DTL.unpack isoUrl')
    withManager $ \manager -> do
      Response _ _ _ bsrc <- http request manager
      bsrc $$ sinkFile (DTL.unpack $ toTextIgnore isoPath')

-----------------------------------------------------------------------------

mirror :: DTL.Text -> DTL.Text
mirror = DTL.append "https://download.joyent.com/pub/iso/"
