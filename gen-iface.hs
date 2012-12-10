{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Language.Haskell.Exts.Annotated as HSE
import Language.Haskell.Modules
import Language.Haskell.Modules.Interfaces
import Language.Haskell.Modules.Flags
import Language.Haskell.Modules.Types
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import qualified Data.Map as Map
import Data.Typeable
import System.FilePath

import Distribution.ModuleName hiding (main)
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.HaskellSuite.Tool
import Distribution.HaskellSuite.Cabal
import Distribution.HaskellSuite.Helpers
import Paths_haskell_modules

data GenIfaceException
  = ParseError HSE.SrcLoc String
  | ScopeErrors (Error HSE.SrcSpan)
  deriving (Show, Typeable)

instance Exception GenIfaceException

fromParseResult :: HSE.ParseResult a -> IO a
fromParseResult (HSE.ParseOk x) = return x
fromParseResult (HSE.ParseFailed loc msg) = throwIO $ ParseError loc msg

main =
  defaultMain theTool

suffix :: String
suffix = "names"

theTool =
  simpleTool
    "haskell-modules"
    version
    (return Nothing)
    compile
    [suffix]

compile buildDir pkgdbs pkgids mods = do
  moduleSet <- forM mods $ \mod ->
    let file = toFilePath (fromString mod) <.> "hs" in
    return . fmap HSE.srcInfoSpan =<< fromParseResult =<< HSE.parseFile file
  let analysis = analyseModules moduleSet
  packages <- readPackagesInfo theTool pkgdbs pkgids
  modData <-
    evalModuleT analysis packages retrieveModuleInfo Map.empty
  forM_ modData $ \(mod, syms) -> do
    let HSE.ModuleName _ modname = getModuleName mod
        ifaceFile = buildDir </> toFilePath (fromString modname) <.> "iface"
    createDirectoryIfMissingVerbose silent True (dropFileName ifaceFile)
    writeInterface ifaceFile syms

-- This function says how we actually find and read the module
-- information, given the search path and the module name
retrieveModuleInfo :: [FilePath] -> ModuleName -> IO Symbols
retrieveModuleInfo dirs name = do
  (base, rel) <- findModuleFile dirs [suffix] name
  readInterface $ base </> rel

getModuleName :: HSE.Module l -> HSE.ModuleName l
getModuleName (HSE.Module _ (Just (HSE.ModuleHead _ mn _ _)) _ _ _) = mn
getModuleName (HSE.XmlPage _ mn _ _ _ _ _) = mn
getModuleName (HSE.XmlHybrid _ (Just (HSE.ModuleHead _ mn _ _)) _ _ _ _ _ _ _) = mn
getModuleName m = HSE.main_mod (HSE.ann m)
