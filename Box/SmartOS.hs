module Box.SmartOS
       ( SmartOS(..)
       , soPlatform
       , soDownload
       , soIsoPath
       , soIsoDirPath
       ) where

import           Crypto.Conduit
import qualified Data.ByteString                 as DB
import qualified Data.ByteString.Base16          as DBB
import qualified Data.ByteString.Char8           as DBC
import qualified Data.ByteString.Lazy            as DBL
import           Data.Conduit
import           Data.Conduit.Binary             hiding (dropWhile, lines)
import           Data.Data
import           Data.Digest.Pure.MD5            hiding (md5)
import qualified Data.Serialize                  as DS
import           Network.HTTP.Conduit            hiding (def, path)
import           System.Directory
import           System.FilePath.Posix

data SmartOS = SmartOS { soIsoName :: FilePath
                         , soIsoMd5  :: String }
              deriving (Data, Show, Typeable, Eq)

soUrl :: String -> String
soUrl = (++) "https://download.joyent.com/pub/iso/"

soIsoUrl :: SmartOS -> String
soIsoUrl SmartOS{..} = soUrl soIsoName

soIsoPath :: SmartOS -> FilePath -> FilePath
soIsoPath SmartOS{..} = flip combine soIsoName

soIsoDirPath :: FilePath -> FilePath
soIsoDirPath = flip combine ".smartos"

soPlatform :: IO SmartOS
soPlatform = do
  bs <- simpleHttp $ soUrl "md5sums.txt"
  let iso    = filter (DBC.isInfixOf $ DBC.pack "iso")
      string = DBC.unpack
      words' = DBC.words
      strict = DB.concat . DBL.toChunks
      lines' = DBC.lines
      latest = map string . words' . last . iso . lines' . strict $ bs
  return $ SmartOS { soIsoMd5 = (latest !! 0), soIsoName = (latest !! 1) }

soDownload :: SmartOS -> FilePath -> IO ()
soDownload pf dir = do
  let url  = soIsoUrl pf
      path = soIsoPath pf dir
  createDirectoryIfMissing True dir
  exists <- doesFileExist path
  if exists
    then do checksum <- checksum path $ soIsoMd5 pf
            case checksum of
              Left hex -> do putStrLn $ "fail! " ++ hex
                             download url path
              _      -> return ()
    else download url path
  where
    download url path = do
      putStrLn $ "downloading " ++ path
      request <- parseUrl url
      withManager $ \manager -> do
        Response _ _ _ bsrc <- http request manager
        bsrc $$ sinkFile path
    checksum path md5 = do
      putStrLn $ "checking " ++ path
      hash <- runResourceT $ sourceFile path $$ sinkHash
      let hex = DBC.unpack $ DBB.encode $ DS.encode (hash :: MD5Digest)
      return $ if md5 /= hex then Left hex else Right md5
