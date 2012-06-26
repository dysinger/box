module Box.SmartOS
       ( SmartOS(..)
       , checkIso
       , downloadIso
       , isoDirPath
       , isoPath
       , isoUrl
       , platform
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
import           System.FilePath.Posix           hiding (FilePath)

-----------------------------------------------------------------------------

data SmartOS = SmartOS { isoName :: FilePath
                       , isoMd5  :: String }
              deriving (Data, Show, Typeable, Eq)

isoUrl :: SmartOS -> String
isoUrl SmartOS{..} = mirror isoName

isoDirPath :: FilePath -> FilePath
isoDirPath = flip combine ".smartos"

isoPath :: SmartOS -> FilePath -> FilePath
isoPath SmartOS{..} = flip combine isoName

platform :: IO SmartOS
platform = do
  bs <- simpleHttp $ mirror "md5sums.txt"
  let iso    = filter (DBC.isInfixOf $ DBC.pack "iso")
      string = DBC.unpack
      words' = DBC.words
      strict = DB.concat . DBL.toChunks
      lines' = DBC.lines
      latest = map string . words' . last . iso . lines' . strict $ bs
  return SmartOS { isoMd5 = (latest !! 0), isoName = (latest !! 1) }

checkIso :: FilePath -> String -> IO (Either String String)
checkIso path md5 = do
  hash <- runResourceT $ sourceFile path $$ sinkHash
  let hex = DBC.unpack $ DBB.encode $ DS.encode (hash :: MD5Digest)
  return $ if md5 /= hex then Left hex else Right md5

downloadIso :: String -> FilePath -> IO ()
downloadIso url path = do
  request <- parseUrl url
  withManager $ \manager -> do
    Response _ _ _ bsrc <- http request manager
    bsrc $$ sinkFile path

-----------------------------------------------------------------------------

mirror :: String -> String
mirror = (++) "https://download.joyent.com/pub/iso/"
