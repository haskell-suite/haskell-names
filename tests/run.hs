{-# LANGUAGE FlexibleInstances, OverlappingInstances, ImplicitParams,
             MultiParamTypeClasses #-}
import Test.Framework hiding (defaultMain)
import Test.Golden
import Test.Golden.Console

import System.FilePath
import System.FilePath.Find
import Data.Monoid
import qualified Data.Map as Map
import Control.Monad.Identity
import Control.Applicative
import Text.Show.Pretty
import Text.Printf
import qualified Data.Foldable as F

import Language.Haskell.Exts.Annotated
import Language.Haskell.Names
import Language.Haskell.Names.Exports
import Language.Haskell.Names.Imports
import Language.Haskell.Names.Annotated
import Language.Haskell.Names.Open
import Language.Haskell.Names.ModuleSymbols
import Language.Haskell.Names.SyntaxUtils
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import Distribution.HaskellSuite
import qualified Distribution.ModuleName as Cabal

import Data.Generics.Traversable
import Data.Proxy

main = defaultMain =<< tests

tests = sequence [exportTests, importTests, annotationTests]

parseAndPrepare file =
  return . fmap srcInfoSpan . fromParseResult =<< parseFile file

getModules = do
  [libraryIface] <-
    (\m -> evalModuleT (fst <$> getInterfaces [m]) [] (error "retrieve") (error "retrieve"))
    =<< parseAndPrepare "tests/exports/Library.hs"
  return $ Map.singleton (convertModuleName "Library") libraryIface

-----------------------------------------------------
-- Export test: parse a source file, dump its symbols
-----------------------------------------------------
exportTest file iface =
  goldenVsFile file golden out run
  where
    golden = file <.> "golden"
    out = file <.> "out"
    run = writeFile out $ ppShow iface

exportTests = do
  mods <- getModules
  testFiles <- find (return True) (extension ==? ".hs") "tests/exports"
  parsed <- mapM parseAndPrepare testFiles
  ifaces <-
    fst <$> runModuleT (fst <$> getInterfaces parsed) [] (error "retrieve") (error "retrieve") mods

  return $ testGroup "exports" $ zipWith exportTest testFiles ifaces

----------------------------------------------------------
-- Import test: parse a source file, dump its global table
----------------------------------------------------------
importTest mods file =
  goldenVsFile file golden out run
  where
    golden = file <.> "golden"
    out = file <.> "out"
    run = do
      imps <- getGlobalTable mods file
      writeFile out $ ppShow imps

getGlobalTable :: Map.Map Cabal.ModuleName Symbols -> FilePath -> IO Global.Table
getGlobalTable mods file = do
  mod <- parseAndPrepare file
  let mImps = snd <$> processImports (getImports mod)
  fst <$> runModuleT mImps [] (error "retrieve") (error "retrieve") mods

importTests = do
  mods <- getModules
  testFiles <- find (return True) (extension ==? ".hs") "tests/imports"
  return $ testGroup "imports" $ map (importTest mods) testFiles

------------------------------------------------------------------
-- Annotation test: parse the source, annotate it and pretty-print
------------------------------------------------------------------
class TestAnn a where
  getAnn :: a -> Maybe (String, Scoped SrcSpan)

instance TestAnn a where
  getAnn = const Nothing

instance TestAnn (QName (Scoped SrcSpan)) where
  getAnn qn = Just (nameToString . qNameToName $ qn, ann qn)

instance GTraversable (Rec TestAnn) (Scoped SrcSpan) where
  gtraverse _ x = pure x

printAnns
  :: Rec TestAnn (a (Scoped SrcSpan))
  => a (Scoped SrcSpan) -> String
printAnns =
  let ?c = Proxy :: Proxy (Rec TestAnn) in
  let
    -- format one annotation
    one :: TestAnn a => a -> String
    one a =
      flip F.foldMap (getAnn a) $ uncurry formatAnn
    -- tie the knot
    go :: Rec TestAnn a => a -> String
    go a = one a ++ gfoldMap go a
  in go

annotationTest file = goldenVsFile file golden out run
  where
    golden = file <.> "golden"
    out = file <.> "out"
    run = do
      mod <- parseAndPrepare file
      let annotatedMod = annotate initialScope mod
      -- writeFile out $ ppShow $ fmap void annotatedMod
      writeFile out $ printAnns annotatedMod

annotationTests = do
  testFiles <- find (return True) (extension ==? ".hs") "tests/annotations"
  return $ testGroup "annotations" $ map annotationTest testFiles

-----------------------
-- Formatting utilities
-----------------------
formatLoc :: SrcInfo l => l -> String
formatLoc srcInfo =
  let loc = getPointLoc srcInfo in
  printf "%d:%d"
    (srcLine   loc)
    (srcColumn loc)

formatInfo :: NameInfo SrcSpan -> String
formatInfo (LocalValue loc) =
  printf "a local value defined at %s" $ formatLoc loc
formatInfo (ScopeError (ENotInScope {})) = "not in scope"
formatInfo None = "none"

formatAnn :: String -> Scoped SrcSpan -> String
formatAnn name (Scoped info loc) =
  printf "%-8s at %4s is %s\n"
    name
    (formatLoc loc)
    (formatInfo info)
