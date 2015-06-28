-- vim:fdm=marker:foldtext=foldtext()
{-# LANGUAGE FlexibleInstances, OverlappingInstances, ImplicitParams,
             MultiParamTypeClasses, FlexibleContexts, GADTs #-}
-- GHC 7.8 fails with the default context stack size of 20
{-# OPTIONS_GHC -fcontext-stack=50 #-}
-- Imports {{{
import Test.Tasty hiding (defaultMain)
import Test.Tasty.Golden
import Test.Tasty.Golden.Manage

import System.FilePath
import System.FilePath.Find
import System.IO
import System.Exit
import Data.Monoid
import Data.List hiding (find)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Identity
import Control.Applicative
import Control.Monad.Trans
import Text.Show.Pretty (ppShow)
import Text.Printf
import qualified Data.Foldable as F

import Language.Haskell.Exts.Annotated hiding (NewType)
import qualified Language.Haskell.Exts.Annotated as Syntax (DataOrNew(NewType))
import Language.Haskell.Names
import Language.Haskell.Names.Interfaces
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
-- }}}

-- Common definitions {{{
type MT = ModuleT [Symbol] IO

main = defaultMain . testGroup "Tests" =<< tests

goldenTest name = goldenVsFileDiff name (\ref new -> ["diff", "-u", ref, new])

-- All tests are created in the same ModuleT session. This means that
-- export tests are available for import in subsequent tests (because of
-- the getIfaces call). However, import tests are not registered in the
-- monad, so they are not available for import.

tests =
  liftM concat . sequence $
    [ evalNamesModuleT (sequence [exportTests, importTests, annotationTests]) []
    ]

parseAndPrepare file =
  return . fmap srcInfoSpan . fromParseResult =<< parseFile file

lang = Haskell2010
exts = [DisableExtension ImplicitPrelude]
getIfaces = getInterfaces lang exts

getTestFiles :: MonadIO m => FilePath -> m [FilePath]
getTestFiles dir = liftIO $ find (return True) (extension ==? ".hs") dir
-- }}}

-----------------------------------------------------
-- Export test: parse a source file, dump its symbols
-----------------------------------------------------
-- {{{
exportTest file iface =
  goldenTest file golden out run
  where
    golden = file <.> "golden"
    out = file <.> "out"
    run = writeBinaryFile out $ ppShow iface

exportTests :: MT TestTree
exportTests = do
  testFiles <- getTestFiles "tests/exports"
  parsed <- liftIO $ mapM parseAndPrepare testFiles
  (ifaces, errors) <- getIfaces parsed

  -- report possible problems
  when (not $ Set.null errors) $ liftIO $ do
    printf "The following unexpected problems were found:\n"
    F.for_ errors $ \e -> do
      putStr $ ppError e
    exitFailure

  return $ testGroup "exports" $ zipWith exportTest testFiles ifaces
-- }}}

----------------------------------------------------------
-- Import test: parse a source file, dump its global table
----------------------------------------------------------
-- {{{
importTest :: FilePath -> Global.Table -> TestTree
importTest file tbl =
  goldenTest file golden out run
  where
    golden = file <.> "golden"
    out = file <.> "out"
    run = do
      writeBinaryFile out $ ppShow tbl

getGlobalTable :: FilePath -> MT Global.Table
getGlobalTable file = do
  mod <- liftIO $ parseAndPrepare file
  let extSet = moduleExtensions lang exts mod
  snd <$> processImports extSet (getImports mod)

importTests :: MT TestTree
importTests = do
  testFiles <- getTestFiles "tests/imports"
  filesAndTables <- forM testFiles $ \file -> (,) file <$> getGlobalTable file
  return $ testGroup "imports" $ map (uncurry importTest) filesAndTables
-- }}}

------------------------------------------------------------------
-- Annotation test: parse the source, annotate it and pretty-print
------------------------------------------------------------------
-- {{{
-- Code to retrieve annotations from the AST using GTraversable
class TestAnn a where
  getAnn :: a -> Maybe (String, Scoped SrcSpan)

instance TestAnn a where
  getAnn = const Nothing

instance TestAnn (QName (Scoped SrcSpan)) where
  getAnn qn = Just (nameToString . qNameToName $ qn, ann qn)

instance TestAnn (Name (Scoped SrcSpan)) where
  getAnn n = Just (nameToString n, ann n)

instance TestAnn (PatField (Scoped SrcSpan)) where
  getAnn (PFieldWildcard l) = Just ("..", l)
  getAnn _ = Nothing

instance TestAnn (FieldUpdate (Scoped SrcSpan)) where
  getAnn (FieldWildcard l) = Just ("..", l)
  getAnn _ = Nothing

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

-- Actual tests
annotationTest file annotatedMod = goldenTest file golden out run
  where
    golden = file <.> "golden"
    out = file <.> "out"
    run = do
      liftIO $ writeBinaryFile out $ printAnns annotatedMod

getAnnotated file = do
  mod <- liftIO $ parseAndPrepare file
  annotateModule lang exts mod

annotationTests = do
  testFiles <- getTestFiles "tests/annotations"
  filesAndMods <- forM testFiles $ \file -> (,) file <$> getAnnotated file
  return $ testGroup "annotations" $
    map (uncurry annotationTest) filesAndMods
-- }}}

-----------------------
-- Formatting utilities
-----------------------
-- {{{
formatLoc :: SrcInfo l => l -> String
formatLoc srcInfo =
  let loc = getPointLoc srcInfo in
  printf "%d:%d"
    (srcLine   loc)
    (srcColumn loc)

formatSymbol Value {} = "value"
formatSymbol Method {} = "method"
formatSymbol Selector {} = "selector"
formatSymbol Constructor {} = "constructor"
formatSymbol Type {} = "type synonym"
formatSymbol Data {} = "data type"
formatSymbol NewType {} = "newtype"
formatSymbol TypeFam {} = "type family"
formatSymbol DataFam {} = "data family"
formatSymbol Class {} = "type class"

formatInfo :: NameInfo SrcSpan -> String
formatInfo (LocalValue loc) =
  printf "a local value defined at %s" $ formatLoc loc
formatInfo (GlobalSymbol symbol _) =
  printf "a global %s, %s"
    (formatSymbol symbol)
    (ppSymbol symbol)
formatInfo ValueBinder = "a value bound here"
formatInfo TypeBinder = "a type or class defined here"
formatInfo (RecPatWildcard symbols) =
  printf
    "a record pattern wildcard which brings the following fields: %s"
    (intercalate ", " $ map ppSymbol symbols)
formatInfo (RecExpWildcard symbols) =
  printf
    "a record construction wildcard which assigns the following fields: %s"
    $ intercalate ", "
      [ printf "%s = (%s)" (prettyPrint field) valueDesc
      | (field, vinfo) <- symbols
      , let valueDesc = formatInfo vinfo
      ]
formatInfo (ScopeError (ENotInScope {})) = "not in scope"
formatInfo (ImportPart symbols) = printf "import part for %s" (intercalate "," (do
        symbol <- symbols
        return (printf "a global %s, %s" (formatSymbol symbol) (ppSymbol symbol))))
formatInfo None = "none"
formatInfo i = error $ "tests/run.hs: formatInfo: " ++ show i

formatAnn :: String -> Scoped SrcSpan -> String
formatAnn name (Scoped info loc) =
  printf "%-8s at %4s is %s\n"
    name
    (formatLoc loc)
    (formatInfo info)
-- }}}
