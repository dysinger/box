module Box.Types.Exception where

import Control.Exception
import Data.Data
import Data.Text.Lazy    (Text)

default (Text)

data Ex = Ex Text
        deriving (Data, Eq, Show, Typeable)

instance Exception Ex
