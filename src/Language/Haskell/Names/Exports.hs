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
import qualified Language.Haskell.Exts as UnAnn (QName(Qual,UnQual))
import Language.Haskell.Exts.Annotated.Simplify (sQName,sModuleName)
import Language.Haskell.Exts.Annotated
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
  case exportSpec of
    EVar _ qn ->
      case Global.lookupValue qn globalTable of
        Global.Error _ -> []
        Global.SymbolFound i -> [i]
        Global.Special {} -> error "Global.Special in export list?"
    EAbs _ _ qn ->
      case Global.lookupType qn globalTable of
        Global.Error _ -> []
        Global.SymbolFound i -> [i]
        Global.Special {} -> error "Global.Special in export list?"
    EThingAll _ qn ->
      case Global.lookupType qn globalTable of
        Global.Error _ -> []
        Global.SymbolFound i -> [i] ++ subs where
          subs = nub (do
            symbol <- concat (Map.elems globalTable)
            Just n' <- return $ symbolParent symbol
            guard (n' == symbolName i)
            return symbol)
        Global.Special {} -> error "Global.Special in export list?"
    EThingWith _ qn cns ->
      case Global.lookupType qn globalTable of
        Global.Error _ -> []
        Global.SymbolFound i -> [i] ++ subs where
            (_, subs) =
              resolveCNames
                (concat (Map.elems globalTable))
                (symbolName i)
                (\cn -> ENotInScope (UnQual (ann cn) (unCName cn))) -- FIXME better error
                cns
        Global.Special {} -> error "Global.Special in export list?"
    -- FIXME ambiguity check
    EModuleContents _ modulename -> exportedSymbols where

        exportedSymbols = Set.toList (
          Set.intersection inScopeQualified inScopeUnqualified)

        inScopeQualified = Set.fromList (do
            (UnAnn.Qual prefix _, symbols) <- Map.toList globalTable
            guard (prefix == sModuleName modulename)
            symbols)

        inScopeUnqualified = Set.fromList (do
            (UnAnn.UnQual _, symbols) <- Map.toList globalTable
            symbols)

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
      Global.Error err ->
        scopeError err exportSpec
      Global.SymbolFound i ->
        EVar (Scoped (Export [i]) l)
            (Scoped (GlobalSymbol i (sQName qn)) <$> qn)
      Global.Special {} -> error "Global.Special in export list?"
  EAbs l ns qn ->
    case Global.lookupType qn globalTable of
      Global.Error err ->
        scopeError err exportSpec
      Global.SymbolFound i ->
        EAbs (Scoped (Export [i]) l)
            (noScope ns)
            (Scoped (GlobalSymbol i (sQName qn)) <$> qn)
      Global.Special {} -> error "Global.Special in export list?"
  EThingAll l qn ->
    case Global.lookupType qn globalTable of
      Global.Error err ->
        scopeError err exportSpec
      Global.SymbolFound i ->
        let
          subs = nub (do
              symbol <- concat (Map.elems globalTable)
              Just n' <- return $ symbolParent symbol
              guard (n' == symbolName i)
              return symbol)
          s = [i] <> subs
        in
          EThingAll (Scoped (Export s) l) (Scoped (GlobalSymbol i (sQName qn)) <$> qn)
      Global.Special {} -> error "Global.Special in export list?"
  EThingWith l qn cns ->
    case Global.lookupType qn globalTable of
      Global.Error err ->
        scopeError err exportSpec
      Global.SymbolFound i ->
        let
          (cns', subs) =
            resolveCNames
              (concat (Map.elems globalTable))
              (symbolName i)
              (\cn -> ENotInScope (UnQual (ann cn) (unCName cn))) -- FIXME better error
              cns
          s = [i] <> subs
        in
          EThingWith (Scoped (Export s) l) (Scoped (GlobalSymbol i (sQName qn)) <$> qn) cns'
      Global.Special {} -> error "Global.Special in export list?"
  -- FIXME ambiguity check
  EModuleContents _ modulename -> Scoped (Export exportedSymbols) <$> exportSpec where

      exportedSymbols = Set.toList (Set.intersection inScopeQualified inScopeUnqualified)

      inScopeQualified = Set.fromList (do
          (UnAnn.Qual prefix _, symbols) <- Map.toList globalTable
          guard (prefix == sModuleName modulename)
          symbols)

      inScopeUnqualified = Set.fromList (do
          (UnAnn.UnQual _, symbols) <- Map.toList globalTable
          symbols)

