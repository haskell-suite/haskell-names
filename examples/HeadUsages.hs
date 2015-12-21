module Main where

import Language.Haskell.Exts.Annotated (
  fromParseResult, parseModuleWithMode, defaultParseMode,
  parseFilename, prettyPrint, srcInfoSpan)
import Language.Haskell.Exts (
  Name(Ident), ModuleName(ModuleName))
import Language.Haskell.Names (
  loadBase, annotate, symbolName,
  Scoped(Scoped), NameInfo(GlobalSymbol))

import qualified Data.Map as Map (
  lookup)

import Data.Maybe (
  fromMaybe, listToMaybe)
import Data.List (
  nub)
import qualified Data.Foldable as Foldable (
  toList)
import Control.Monad (
  forM_, guard)

main :: IO ()
main = do

  -- read the program's source from stdin
  source <- getContents

  -- parse the program (using haskell-src-exts)
  let ast = fromParseResult (
        parseModuleWithMode defaultParseMode {parseFilename="stdin"} source)

  -- get base environment
  baseEnvironment <- loadBase

  -- get symbols defined in prelude
  let preludeSymbols = fromMaybe (error "Prelude not found") (
        Map.lookup (ModuleName "Prelude") baseEnvironment)

  -- find a Prelude symbol with name 'head' using the List monad
  let headSymbol = fromMaybe (error "Prelude.head not found") (
        listToMaybe (do
          preludeSymbol <- preludeSymbols
          guard (symbolName preludeSymbol == Ident "head")
          return preludeSymbol))

  -- annotate the AST
  let annotatedAST = annotate baseEnvironment ast

  -- get all annotations
  let annotations = Foldable.toList annotatedAST

  -- filter head Usages in List monad and remove duplicates
  let headUsages = nub (do
        Scoped (GlobalSymbol globalSymbol _) location <- annotations
        guard (globalSymbol == headSymbol)
        return location)

  case headUsages of
    [] ->
      putStrLn "Congratulations! Your code doesn't use Prelude.head"
    _ -> forM_ headUsages (\location ->
      putStrLn ("Prelude.head is used at " ++ (prettyPrint (srcInfoSpan location))))

