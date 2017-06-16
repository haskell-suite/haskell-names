module ExportAmbiguous (DataType(..)) where

import qualified Prelude (DataType(..))
import Prelude hiding (DataType(..))

data DataType = Constructor1 | Constructor2
