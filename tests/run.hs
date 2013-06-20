{-# LANGUAGE FlexibleInstances, OverlappingInstances, ImplicitParams,
             MultiParamTypeClasses #-}
import Test.Framework hiding (defaultMain)
import Test.Golden
import Test.Golden.Console

import System.FilePath
import System.FilePath.Find
import System.Exit
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Identity
import Control.Applicative
import Control.Monad.Trans
import Text.Show.Pretty
import Text.Printf
import qualified Data.Foldable as F

import Language.Haskell.Exts.Annotated
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

type MT = ModuleT Symbols IO

main = defaultMain =<< tests

tests =
  liftM concat . sequence $
    [ evalNamesModuleT (sequence [exportTests, importTests]) []
    , sequence [annotationTests]
    ]

parseAndPrepare file =
  return . fmap srcInfoSpan . fromParseResult =<< parseFile file

lang = Haskell2010
exts = [DisableExtension ImplicitPrelude]
getIfaces = getInterfaces lang exts

-----------------------------------------------------
-- Export test: parse a source file, dump its symbols
-----------------------------------------------------
exportTest file iface =
  goldenVsFile file golden out run
  where
    golden = file <.> "golden"
    out = file <.> "out"
    run = writeFile out $ ppShow iface

exportTests :: MT Test
exportTests = do
  testFiles <- liftIO $ find (return True) (extension ==? ".hs") "tests/exports"
  parsed <- liftIO $ mapM parseAndPrepare testFiles
  (ifaces, errors) <- getIfaces parsed

  -- report possible problems
  when (not $ Set.null errors) $ liftIO $ do
    printf "The following unexpected problems were found:\n"
    F.for_ errors $ \e -> do
      putStr $ ppError e
    exitFailure

  return $ testGroup "exports" $ zipWith exportTest testFiles ifaces

----------------------------------------------------------
-- Import test: parse a source file, dump its global table
----------------------------------------------------------
importTest :: FilePath -> Global.Table -> Test
importTest file tbl =
  goldenVsFile file golden out run
  where
    golden = file <.> "golden"
    out = file <.> "out"
    run = do
      writeFile out $ ppShow tbl

getGlobalTable :: FilePath -> MT Global.Table
getGlobalTable file = do
  mod <- liftIO $ parseAndPrepare file
  let extSet = moduleExtensions lang exts mod
  snd <$> processImports extSet (getImports mod)

importTests :: MT Test
importTests = do
  testFiles <- liftIO $ find (return True) (extension ==? ".hs") "tests/imports"
  filesAndTables <- forM testFiles $ \file -> (,) file <$> getGlobalTable file
  return $ testGroup "imports" $ map (uncurry importTest) filesAndTables

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
