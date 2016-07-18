{-# LANGUAGE FlexibleInstances, OverlappingInstances, ImplicitParams,
             MultiParamTypeClasses, FlexibleContexts, GADTs #-}
-- GHC 7.8 fails with the default context stack size of 20
{-# OPTIONS_GHC -fcontext-stack=50 #-}
import Test.Tasty hiding (defaultMain)
import Test.Tasty.Golden
import Test.Tasty.Golden.Manage

import System.FilePath
import System.FilePath.Find
import System.IO
import System.Exit
import Data.Monoid
import Data.List hiding (find)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Identity
import Control.Applicative
import Control.Monad.Trans
import Text.Show.Pretty (ppShow)
import Text.Printf
import qualified Data.Foldable as F

import Language.Haskell.Exts hiding (DataOrNew(NewType))
import qualified Language.Haskell.Exts.Syntax as Syntax (DataOrNew(NewType))
import qualified Language.Haskell.Exts as U (ModuleName(ModuleName))
import Language.Haskell.Names
import Language.Haskell.Names.Exports
import Language.Haskell.Names.Imports
import Language.Haskell.Names.Annotated
import Language.Haskell.Names.Open
import Language.Haskell.Names.ModuleSymbols
import Language.Haskell.Names.SyntaxUtils
import qualified Language.Haskell.Names.GlobalSymbolTable as Global

import Data.Generics.Traversable
import Data.Proxy


main :: IO ()
main = do
  exportTestModules <- getTestModules "tests/exports"
  importTestModules <- getTestModules "tests/imports"
  annotationTestModules <- getTestModules "tests/annotations"
  let environment = resolve (map snd exportTestModules) Map.empty
  defaultMain (testGroup "Tests" [
    exportTests environment exportTestModules,
    importTests environment importTestModules,
    annotationTests environment annotationTestModules,
    environmentTests])

getTestModules :: FilePath -> IO [(FilePath, Module SrcSpan)]
getTestModules directory = do
  paths <- find (return True) (extension ==? ".hs") directory
  forM paths (\path -> do
    result <- parseFile path
    return (path, fmap srcInfoSpan (fromParseResult result)))

exportTests :: Environment -> [(FilePath, Module SrcSpan)] -> TestTree
exportTests environment exportTestModules =
  testGroup "exports" (map (exportTest environment) exportTestModules)

-- | Dump exported symbols.
-- TODO: check for errors during resolution.
exportTest :: Environment -> (FilePath, Module SrcSpan) -> TestTree
exportTest environment (path, modul) = goldenTest path run where
  out = path <.> "out"
  run = writeBinaryFile out (ppShow exports)
  exports = exportedSymbols globalTable modul
  globalTable = moduleTable (importTable environment modul) modul

importTests :: Environment -> [(FilePath, Module SrcSpan)] -> TestTree
importTests environment importTestModules =
  testGroup "imports" (map (importTest environment) importTestModules)

-- | Dump global table.
importTest :: Environment -> (FilePath, Module SrcSpan) -> TestTree
importTest environment (path, modul) = goldenTest path run where
  out = path <.> "out"
  run = writeBinaryFile out (ppShow table)
  table = importTable environment modul

annotationTests :: Environment -> [(FilePath, Module SrcSpan)] -> TestTree
annotationTests environment annotationTestModules =
  testGroup "annotations" (map (annotationTest environment) annotationTestModules)

-- | Annotate and pretty print the annotations.
annotationTest :: Environment -> (FilePath, Module SrcSpan) -> TestTree
annotationTest environment (path, modul) = goldenTest path run where
  out = path <.> "out"
  run = writeBinaryFile out (printAnns annotatedModule)
  annotatedModule = annotate environment modul

environmentTests :: TestTree
environmentTests = goldenTest path run where
  run = do
    baseEnvironment <- loadBase
    writeSymbols out (baseEnvironment Map.! (U.ModuleName () "Prelude"))
  path = "tests/environment/Prelude.symbols"
  out = path <.> "out"

goldenTest :: FilePath -> IO () -> TestTree
goldenTest path = goldenVsFileDiff
  path
  (\ref new -> ["diff", "-u", ref, new])
  (path <.> "golden")
  (path <.> "out")


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
