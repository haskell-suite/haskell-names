{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Language.Haskell.Exts.Annotated as HSE
import Language.Haskell.Modules
import Language.Haskell.Modules.Interfaces
import Language.Haskell.Modules.Flags
import Language.Haskell.Modules.ModuleSummary
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Data.Foldable
import qualified Data.Map as Map
import Data.Typeable
import System.FilePath

import Distribution.HaskellSuite.Tool
import Distribution.HaskellSuite.Cabal
import Distribution.HaskellSuite.Helpers
import Paths_haskell_modules

data GenIfaceException
  = ParseError HSE.SrcLoc String
  | ScopeErrors [Msg]
  | ModuleNotFound ModuleName
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
    (return "/dev/null")
    compile
    [suffix]

compile buildDir pkgdbs pkgids files = do
  moduleSet <- forM files $ \file ->
    return . fmap HSE.srcInfoSpan =<< fromParseResult =<< HSE.parseFile file
  let analysis = scopeAnalysis getModuleInfo' defaultFlags moduleSet
  packages <- readPackagesInfo theTool pkgdbs pkgids
  (msgs, scopedModules) <-
    evalModuleT analysis packages retrieveModuleInfo Map.empty
  let errs = filter isError msgs
  when (not $ null errs) $
    throwIO $ ScopeErrors errs
  undefined

-- This functions works in the ModuleT monad and will be actually called
-- by the scope analysis
getModuleInfo' :: HSE.ModuleName () -> ModuleT ModuleSummary IO ModuleSummary
getModuleInfo' (HSE.ModuleName _ modStr) = do
  let modName = fromString modStr
  mbModInfo <- getModuleInfo modName
  maybe (liftIO $ throwIO $ ModuleNotFound modName) return mbModInfo

-- This function says how we actually find and read the module
-- information, given the search path and the module name
retrieveModuleInfo :: [FilePath] -> ModuleName -> IO ModuleSummary
retrieveModuleInfo dirs name = do
  (base, rel) <- findModuleFile dirs [suffix] name
  readInterface $ base </> rel

{-
  mod <- fromParseResult <$> parseFile f
  let sc = snd $ runS defaultFlags $
        (scopeModule $ fmap srcInfoSpan mod) >> getModules
  forM_ (Map.toList $ sc) $ \ms -> writeInterface i $ makeIface ms
-}
