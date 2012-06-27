module Shell ( module Shell
             , module Shelly
             ) where

import qualified Data.Text.Lazy   as DTL
import           System.Directory

import qualified Prelude          as P
import           Prelude          hiding (FilePath)
import           Shelly

default (DTL.Text)

-----------------------------------------------------------------------------

homePath :: ShIO FilePath
homePath = return . fromText . DTL.pack =<< liftIO getHomeDirectory

status :: DTL.Text -> ShIO a -> ShIO a
status msg shio = do
  echo msg
  x <- shio
  return x

txtToStr :: DTL.Text -> String
txtToStr = DTL.unpack

strToTxt :: String -> DTL.Text
strToTxt = DTL.pack

fpToTxt :: FilePath -> DTL.Text
fpToTxt = toTextIgnore

txtToFp :: DTL.Text -> FilePath
txtToFp = fromText

fpToGfp :: FilePath -> P.FilePath
fpToGfp = txtToStr . fpToTxt

gfpToFp :: P.FilePath -> FilePath
gfpToFp = txtToFp . strToTxt

toText :: forall a. Show a => a -> DTL.Text
toText = DTL.pack . show

toLower :: forall a. Show a => a -> DTL.Text
toLower = DTL.toLower . toText
