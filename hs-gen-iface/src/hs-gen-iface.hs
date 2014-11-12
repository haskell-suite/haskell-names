{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Language.Haskell.Exts.Annotated as HSE
import qualified Language.Haskell.Exts as UnAnn
import Language.Haskell.Exts (defaultParseMode, ParseMode(..))
import Language.Haskell.Names
import Language.Haskell.Names.Interfaces
import Language.Haskell.Exts.Extension
import Language.Haskell.Exts.SrcLoc
import Control.Monad
import Control.Exception
import Data.Typeable
import Data.Proxy
import Data.Maybe
import qualified Data.Foldable as F
import System.FilePath
import Text.Printf

import Distribution.HaskellSuite
import qualified Distribution.HaskellSuite.Compiler as Compiler

import Distribution.ModuleName hiding (main)
import Distribution.Simple.Utils
import Distribution.Verbosity

import Language.Haskell.Exts.Annotated.CPP
import Paths_hs_gen_iface
import Language.Haskell.Names.SyntaxUtils

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

parse :: Language -> [Extension] -> CpphsOptions -> FilePath -> IO (HSE.Module HSE.SrcSpan)
parse lang exts cppOpts file = do
    x <- return . fmap HSE.srcInfoSpan . fst
            =<< fromParseResult
            =<< parseFileWithCommentsAndCPP (fixCppOpts cppOpts) mode file
    return x
  where
    mode = defaultParseMode
             { UnAnn.parseFilename   = file
             , baseLanguage          = lang
             , extensions            = exts
             , ignoreLanguagePragmas = False
             , ignoreLinePragmas     = False
             }

compile :: Compiler.CompileFn
compile buildDir mbLang exts cppOpts pkgName pkgdbs deps files = do
  let lang = fromMaybe Haskell98 mbLang

  moduleSet <- mapM (parse lang exts cppOpts) files

  packages <- readPackagesInfo (Proxy :: Proxy NamesDB) pkgdbs deps

  (ifaces, errors) <- evalModuleT (getInterfaces lang exts moduleSet) packages "names" readInterface

  F.for_ errors $ \e -> printf "Warning: %s" (ppError e)

  forM_ (zip moduleSet ifaces) $ \(mod, syms) -> do

    let HSE.ModuleName _ modname = getModuleName mod
        ifaceFile = buildDir </> toFilePath (fromString modname) <.> suffix

    createDirectoryIfMissingVerbose silent True (dropFileName ifaceFile)

    writeInterface ifaceFile syms
