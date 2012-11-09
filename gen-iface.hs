module Main where
import Language.Haskell.Exts.Annotated
import Language.Haskell.Modules
import Language.Haskell.Modules.Interfaces
import Language.Haskell.Modules.Flags
import System.Environment
import Control.Applicative
import Data.Foldable
import qualified Data.Map as Map

main = do
  [f,i] <- getArgs
  mod <- fromParseResult <$> parseFile f
  let sc = snd $ runS defaultFlags $
        (scopeModule $ fmap srcInfoSpan mod) >> getModules
  forM_ (Map.toList $ sc) $ \ms -> writeInterface i $ makeIface ms
