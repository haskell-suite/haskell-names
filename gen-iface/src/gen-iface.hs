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
import qualified Data.Map as Map
import Data.Typeable
import System.FilePath
import Text.Printf

import Distribution.ModuleName hiding (main)
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.HaskellSuite.Tool
import Distribution.HaskellSuite.Cabal
import Distribution.HaskellSuite.Helpers
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
  defaultMain theTool

suffix :: String
suffix = "names"

theTool :: SimpleTool
theTool =
  simpleTool
    "haskell-modules"
    version
    [minBound..maxBound]
    (return Nothing)
    compile
    [suffix]

fixCppOpts :: CpphsOptions -> CpphsOptions
fixCppOpts opts =
  opts {
    defines = ("__GLASGOW_HASKELL__", "") : defines opts -- FIXME
  }

parse :: [Extension] -> CpphsOptions -> FilePath -> IO (HSE.Module HSE.SrcSpan)
parse exts cppOpts file = do
    putStrLn $ "Parsing: " ++ file
    -- FIXME: use parseFileWithMode?
    x <- return . fmap HSE.srcInfoSpan . fst
            =<< fromParseResult
            =<< parseFileWithCommentsAndCPP (fixCppOpts cppOpts) mode file
    putStrLn $ "Parsed: " ++ file
    return x
  where
    mode = defaultParseMode
             { UnAnn.parseFilename   = file
             , extensions            = exts
             , ignoreLanguagePragmas = False
             , ignoreLinePragmas     = False
             }

compile :: [Char]
        -> [Extension]
        -> CpphsOptions
        -> [Distribution.Simple.Compiler.PackageDB]
        -> [Distribution.Package.InstalledPackageId]
        -> [FilePath]
        -> IO ()
compile buildDir exts cppOpts pkgdbs pkgids files = do
  moduleSet <- mapM (parse exts cppOpts) files
  let analysis = analyseModules moduleSet
  packages <- readPackagesInfo theTool pkgdbs pkgids
  modData <-
    evalModuleT analysis packages retrieveModuleInfo Map.empty
  forM_ modData $ \(mod, syms) -> do
    let HSE.ModuleName _ modname = getModuleName mod
        ifaceFile = buildDir </> toFilePath (fromString modname) <.> suffix
    createDirectoryIfMissingVerbose silent True (dropFileName ifaceFile)
    writeInterface ifaceFile syms

-- This function says how we actually find and read the module
-- information, given the search path and the module name
retrieveModuleInfo :: [FilePath] -> ModuleName -> IO Symbols
retrieveModuleInfo dirs name = do
  (base, rel) <- findModuleFile dirs [suffix] name
  readInterface $ base </> rel
