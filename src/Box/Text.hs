module Box.Text where

import qualified Data.ByteString.Base16  as BB
import qualified Data.ByteString.Char8   as BC
import qualified Data.ByteString.Lazy    as B
import qualified Data.Serialize          as S
import           Data.Text.Lazy          (Text)
import qualified Data.Text.Lazy          as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Prelude                 as P
import           Prelude                 hiding (FilePath)
import           Shelly

default (Text)

-----------------------------------------------------------------------------

txtToStr :: Text -> String
txtToStr = T.unpack

strToTxt :: String -> Text
strToTxt = T.pack

fpToTxt :: FilePath -> Text
fpToTxt = toTextIgnore

txtToFp :: Text -> FilePath
txtToFp = fromText

fpToGfp :: FilePath -> P.FilePath
fpToGfp = txtToStr . fpToTxt

gfpToFp :: P.FilePath -> FilePath
gfpToFp = txtToFp . strToTxt

toTxt :: forall a. Show a => a -> Text
toTxt = strToTxt . show

toLower :: Text -> Text
toLower = T.toLower

toLowerTxt :: forall a. Show a => a -> Text
toLowerTxt = toLower . toTxt

txtToLowerStr :: Text -> String
txtToLowerStr = T.unpack . toLower

bsToTxt :: B.ByteString -> Text
bsToTxt = T.decodeUtf8

bsToTxtLines :: B.ByteString -> [Text]
bsToTxtLines = T.lines . bsToTxt

toHexTxt :: forall a. S.Serialize a => a -> Text
toHexTxt = T.pack . BC.unpack . BB.encode . S.encode
