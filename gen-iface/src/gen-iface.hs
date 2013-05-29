{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Language.Haskell.Exts.Annotated as HSE
import qualified Language.Haskell.Exts as UnAnn
import Language.Haskell.Exts (defaultParseMode, ParseMode(..))
import Language.Haskell.Modules
import Language.Haskell.Modules.Interfaces
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc
import Control.Monad
import Control.Exception
import Data.Typeable
import Data.Proxy
import qualified Data.Foldable as F
import qualified Data.Set as Set
import System.FilePath
import Text.Printf

import Distribution.HaskellSuite
import qualified Distribution.HaskellSuite.Compiler as Compiler

import Distribution.ModuleName hiding (main)
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Simple.Compiler (PackageDB)
import Distribution.Package (InstalledPackageId)

import Language.Haskell.Exts.Annotated.CPP
import Paths_gen_iface
import Language.Haskell.Modules.SyntaxUtils

data GenIfaceException
  = ParseError HSE.SrcLoc String
  | ScopeErrors (Error HSE.SrcSpan)
  deriving Typeable

instance Show GenIfaceException where
  show (ParseError (SrcLoc file line col) msg) =
    printf "%s:%d:%d:\n  %s" file line col msg
  show ScopeErrors {} = "scope errors (show not implemented yet)"

instance Exception GenIfaceException

fromParseResult :: HSE.ParseResult a -> IO a
fromParseResult (HSE.ParseOk x) = return x
fromParseResult (HSE.ParseFailed loc msg) = throwIO $ ParseError loc msg

main :: IO ()
main =
  Compiler.main theTool

suffix :: String
suffix = "names"

theTool :: Compiler.Simple NamesDB
theTool =
  Compiler.simple
    "haskell-names"
    version
    knownLanguages
    knownExtensions
    compile
    [suffix]

fixCppOpts :: CpphsOptions -> CpphsOptions
fixCppOpts opts =
  opts {
    defines = ("__GLASGOW_HASKELL__", "706") : defines opts -- FIXME
  }

parse :: [Extension] -> CpphsOptions -> FilePath -> IO (HSE.Module HSE.SrcSpan)
parse exts cppOpts file = do
    -- FIXME: use parseFileWithMode?
    x <- return . fmap HSE.srcInfoSpan . fst
            =<< fromParseResult
            =<< parseFileWithCommentsAndCPP (fixCppOpts cppOpts) mode file
    return x
  where
    mode = defaultParseMode
             { UnAnn.parseFilename   = file
             , extensions            = exts
             , ignoreLanguagePragmas = False
             , ignoreLinePragmas     = False
             }

-- FIXME use the language argument
compile :: Compiler.CompileFn
compile buildDir mbLang exts cppOpts pkgName pkgdbs deps files = do
  moduleSet <- mapM (parse exts cppOpts) files
  let analysis = analyseModules moduleSet
  packages <- readPackagesInfo (Proxy :: Proxy NamesDB) pkgdbs deps
  modData <-
    evalModuleT analysis packages "names" readInterface
  forM_ modData $ \(mod, syms) -> do
    let HSE.ModuleName _ modname = getModuleName mod
        ifaceFile = buildDir </> toFilePath (fromString modname) <.> suffix
        errors = Set.unions
          [ F.foldMap getErrors $ getImports mod
          , F.foldMap getErrors $ getExportSpecList mod
          ]
    F.for_ errors $ \e -> printf "Warning: %s\n" (show e)
    createDirectoryIfMissingVerbose silent True (dropFileName ifaceFile)
    writeInterface ifaceFile $ qualifySymbols pkgName syms
