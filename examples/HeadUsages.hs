module Main where

import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Exts as UnAnn (Name(Ident))
import Language.Haskell.Names
import Language.Haskell.Names.Interfaces
import Distribution.HaskellSuite
import Distribution.Simple.Compiler

import Data.Maybe
import Data.List
import Data.Proxy
import qualified Data.Foldable as Foldable
import Text.Printf
import Control.Applicative
import Control.Monad

main :: IO ()
main = do

  -- read the program's source from stdin
  source <- getContents

  let
    -- parse the program (using haskell-src-exts)
    ast = fromParseResult $
      parseModuleWithMode defaultParseMode {parseFilename="stdin"} source

  -- get all installed packages (user and global)
  pkgs <-
    (++) <$>
      getInstalledPackages (Proxy :: Proxy NamesDB) UserPackageDB <*>
      getInstalledPackages (Proxy :: Proxy NamesDB) GlobalPackageDB

  headUsages <- evalNamesModuleT (findHeads ast) pkgs

  forM_ headUsages $ \loc ->
    printf "Prelude.head is used at %s\n" (prettyPrint $ srcInfoSpan loc)

  when (null headUsages) $
    printf "Congratulations! Your code doesn't use Prelude.head\n"

-- this is a computation in a ModuleT monad, because we need access to
-- modules' interfaces
findHeads :: Module SrcSpanInfo -> ModuleT [Symbol] IO [SrcSpanInfo]
findHeads ast = do
  -- first of all, figure out the canonical name of "Prelude.head"
  -- (hint: it's "GHC.List.head")
  symbols <- fromMaybe (error "Prelude not found") <$> getModuleInfo "Prelude"
  let
    -- we walk through all values defined in Prelude and look for
    -- one with name "head"
    headSymbol =
      fromMaybe (error "Prelude.head not found") (listToMaybe (do
        symbol <- symbols
        guard (symbolName symbol == UnAnn.Ident "head")
        return symbol))

  -- annotate our ast with name binding information
  annotatedAst <-
    annotateModule
      Haskell2010 -- base language
      []          -- set of extensions
      ast


  let
    -- get list of all annotations
    annotations = Foldable.toList annotatedAst

    -- look for headSymbol
    headUsages = nub (do
      Scoped (GlobalSymbol globalSymbol _) location <- annotations
      guard (globalSymbol == headSymbol)
      return location)

  return headUsages