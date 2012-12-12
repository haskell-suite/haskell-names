import Test.Framework
import Test.Golden

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as BS

import System.FilePath
import System.FilePath.Find
import Data.Monoid
import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Applicative
import Text.Show.Pretty

import Language.Haskell.Exts.Annotated
import Language.Haskell.Modules.Exports
import Language.Haskell.Modules.ModuleSymbols
import qualified Language.Haskell.Modules.GlobalSymbolTable as Global
import Distribution.HaskellSuite.Helpers

main = defaultMain =<< tests

tests = sequence [exportTests]

-- Export test: parse a source file, dumb its symbols
exportTest file =
  goldenVsFile file golden out run
  where
    golden = file <.> "golden"
    out = file <.> "out"
    run = do
      mod <-
        return . fmap srcInfoSpan . fromParseResult =<<
        parseFile file
      let mExps = snd <$> processExports (moduleTable mod) mod
          exps = runIdentity $
            evalModuleT mExps [] (error "retrieve") Map.empty
      BS.writeFile out $ BS.fromString $ ppShow exps

exportTests = do
  testFiles <- find (return True) (extension ==? ".hs") "tests/exports"
  return $ testGroup "exports" $ map exportTest testFiles
