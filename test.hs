import Test.Framework hiding (defaultMain)
import Test.Golden
import Test.Golden.Console

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
import Language.Haskell.Modules.Types
import Language.Haskell.Modules.Exports
import Language.Haskell.Modules.ModuleSymbols
import qualified Language.Haskell.Modules.GlobalSymbolTable as Global
import Distribution.HaskellSuite.Helpers
import qualified Distribution.ModuleName as Cabal

main = defaultMain =<< tests

tests = sequence [exportTests]

-- Export test: parse a source file, dumb its symbols
exportTest mods file =
  goldenVsFile file golden out run
  where
    golden = file <.> "golden"
    out = file <.> "out"
    run = do
      exps <- getInterface mods file
      BS.writeFile out $ BS.fromString $ ppShow exps

getInterface :: Map.Map Cabal.ModuleName Symbols -> FilePath -> IO Symbols
getInterface mods file = do
      mod <-
        return . fmap srcInfoSpan . fromParseResult =<<
        parseFile file
      let mExps = snd <$> processExports (moduleTable mod) mod
          exps = runIdentity $
            evalModuleT mExps [] (error "retrieve") mods
      return exps

exportTests = do
  libraryIface <- getInterface mempty "tests/exports/Library.hs"
  let mods = Map.singleton (convertModuleName "Library") libraryIface
  testFiles <- find (return True) (extension ==? ".hs") "tests/exports"
  return $ testGroup "exports" $ map (exportTest mods) testFiles
