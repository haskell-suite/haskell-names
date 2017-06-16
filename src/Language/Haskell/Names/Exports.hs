{-# LANGUAGE TypeFamilies, NoMonoLocalBinds #-}
module Language.Haskell.Names.Exports
  ( exportedSymbols
  , annotateExportSpecList
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Data
import Language.Haskell.Exts
import Language.Haskell.Names.Types
import Language.Haskell.Names.ScopeUtils
import Language.Haskell.Names.SyntaxUtils
import Language.Haskell.Names.ModuleSymbols
import Language.Haskell.Names.GlobalSymbolTable as Global
import Data.List (nub)


-- | Compute the list of symbols the given module exports using the given
-- table of symbols that are in scope in that module.
exportedSymbols :: (Data l, Eq l) => Global.Table -> Module l -> [Symbol]
exportedSymbols globalTable modul = case getExportSpecList modul of
  Nothing -> moduleSymbols globalTable modul
  Just (ExportSpecList _ exportSpecs) ->
    concatMap (exportSpecSymbols globalTable) exportSpecs

exportSpecSymbols :: Global.Table -> ExportSpec l -> [Symbol]
exportSpecSymbols globalTable exportSpec =
  case annotateExportSpec globalTable exportSpec of
    EVar (Scoped (Export symbols) _) _ -> symbols
    EAbs (Scoped (Export symbols) _) _ _ -> symbols
    EThingWith (Scoped (Export symbols) _) _ _ _ -> symbols
    EModuleContents (Scoped (Export symbols) _) _ -> symbols
    _ -> []

-- | Annotate the given export list with scoping information using the given
-- table of symbols that are in scope in that module.
annotateExportSpecList :: Global.Table -> ExportSpecList l -> ExportSpecList (Scoped l)
annotateExportSpecList globalTable (ExportSpecList l exportSpecs) =
  ExportSpecList (none l) (map (annotateExportSpec globalTable) exportSpecs)

annotateExportSpec :: Global.Table -> ExportSpec l -> ExportSpec (Scoped l)
annotateExportSpec globalTable exportSpec =
 case exportSpec of
  EVar l qn ->
    case Global.lookupValue qn globalTable of
      [] -> scopeError (ENotInScope qn) exportSpec
      [symbol] -> EVar (Scoped (Export [symbol]) l)
            (Scoped (GlobalSymbol symbol (dropAnn qn)) <$> qn)
      symbols -> scopeError (EAmbiguous qn symbols) exportSpec
  EAbs l ns@(PatternNamespace _) qn ->
    case Global.lookupValue qn globalTable of
      [] -> scopeError (ENotInScope qn) exportSpec
      [symbol] -> EAbs (Scoped (Export [symbol]) l)
            (noScope ns)
            (Scoped (GlobalSymbol symbol (dropAnn qn)) <$> qn)
      symbols -> scopeError (EAmbiguous qn symbols) exportSpec
  EAbs l ns qn ->
    case Global.lookupType qn globalTable of
      [] -> scopeError (ENotInScope qn) exportSpec
      [symbol] -> EAbs (Scoped (Export [symbol]) l)
            (noScope ns)
            (Scoped (GlobalSymbol symbol (dropAnn qn)) <$> qn)
      symbols -> scopeError (EAmbiguous qn symbols) exportSpec
  EThingWith l w@(EWildcard _ _) qn _ ->
    case Global.lookupType qn globalTable of
      [] -> scopeError (ENotInScope qn) exportSpec
      [symbol] ->
        let
          subSymbols = nub (do
              subSymbol <- concat (Map.elems globalTable)
              Just subSymbolParentName <- return $ symbolParent subSymbol
              guard (subSymbolParentName == symbolName symbol)
              guard (symbolModule subSymbol == symbolModule symbol)
              return subSymbol)
          s = [symbol] <> subSymbols
        in
          EThingWith (Scoped (Export s) l) (fmap (Scoped None) w) (Scoped (GlobalSymbol symbol (dropAnn qn)) <$> qn) []
      symbols -> scopeError (EAmbiguous qn symbols) exportSpec
  EThingWith l w@(NoWildcard {}) qn cns ->
    case Global.lookupType qn globalTable of
      [] -> scopeError (ENotInScope qn) exportSpec
      [symbol] ->
        let
          (cns', subSymbols) =
            resolveCNames
              (concat (Map.elems globalTable))
              (symbolName symbol)
              (\cn -> ENotInScope (UnQual (ann cn) (unCName cn))) -- FIXME better error
              cns
          s = [symbol] <> subSymbols
        in
          EThingWith (Scoped (Export s) l) (fmap (Scoped None) w) (Scoped (GlobalSymbol symbol (dropAnn qn)) <$> qn) cns'
      symbols -> scopeError (EAmbiguous qn symbols) exportSpec
  -- FIXME ambiguity check
  EModuleContents _ modulename -> Scoped (Export exportedSymbols) <$> exportSpec where

      exportedSymbols = Set.toList (Set.intersection inScopeQualified inScopeUnqualified)

      inScopeQualified = Set.fromList (do
          (Qual _ prefix _, symbols) <- Map.toList globalTable
          guard (prefix == dropAnn modulename)
          symbols)

      inScopeUnqualified = Set.fromList (do
          (UnQual _ _, symbols) <- Map.toList globalTable
          symbols)

