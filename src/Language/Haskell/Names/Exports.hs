{-# LANGUAGE TypeFamilies, NoMonoLocalBinds #-}
module Language.Haskell.Names.Exports where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Writer
import Data.Data
import Distribution.HaskellSuite.Modules
import qualified Language.Haskell.Exts as UnAnn (QName(Qual,UnQual))
import Language.Haskell.Exts.Annotated.Simplify (sQName,sModuleName)
import Language.Haskell.Exts.Annotated
import Language.Haskell.Names.Types
import Language.Haskell.Names.ScopeUtils
import Language.Haskell.Names.SyntaxUtils
import Language.Haskell.Names.ModuleSymbols
import Language.Haskell.Names.GlobalSymbolTable as Global
import Data.List (nub)

processExports
  :: (MonadModule m, ModuleInfo m ~ [Symbol], Data l, Eq l)
  => Global.Table
  -> Module l
  -> m (Maybe (ExportSpecList (Scoped l)), [Symbol])
processExports tbl m =
  case getExportSpecList m of
    Nothing ->
      return (Nothing, moduleSymbols tbl m)
    Just exp ->
      liftM (first Just) $ resolveExportSpecList tbl exp

resolveExportSpecList
  :: (MonadModule m, ModuleInfo m ~ [Symbol])
  => Global.Table
  -> ExportSpecList l
  -> m (ExportSpecList (Scoped l), [Symbol])
resolveExportSpecList tbl (ExportSpecList l specs) =
  liftM (first $ ExportSpecList $ none l) $
  runWriterT $
  mapM (WriterT . resolveExportSpec tbl) specs

resolveExportSpec
  :: (MonadModule m, ModuleInfo m ~ [Symbol])
  => Global.Table
  -> ExportSpec l
  -> m (ExportSpec (Scoped l), [Symbol])
resolveExportSpec tbl exp =
  case exp of
    EVar l qn -> return $
      case Global.lookupValue qn tbl of
        Global.Error err ->
          (scopeError err exp, mempty)
        Global.SymbolFound i ->
            (EVar (Scoped (Export [i]) l)
              (Scoped (GlobalSymbol i (sQName qn)) <$> qn), [i])
        Global.Special {} -> error "Global.Special in export list?"
    EAbs l ns qn -> return $
      case Global.lookupType qn tbl of
        Global.Error err ->
          (scopeError err exp, mempty)
        Global.SymbolFound i ->
            (EAbs (Scoped (Export [i]) l)
              (noScope ns)
              (Scoped (GlobalSymbol i (sQName qn)) <$> qn), [i])
        Global.Special {} -> error "Global.Special in export list?"
    EThingAll l qn -> return $
      case Global.lookupType qn tbl of
        Global.Error err ->
          (scopeError err exp, mempty)
        Global.SymbolFound i ->
          let
            subs = nub (do
                symbol <- concat (Map.elems tbl)
                Just n' <- return $ symbolParent symbol
                guard (n' == symbolName i)
                return symbol)
            s = [i] <> subs
          in
            ( EThingAll (Scoped (Export s) l) (Scoped (GlobalSymbol i (sQName qn)) <$> qn)
            , s
            )
        Global.Special {} -> error "Global.Special in export list?"
    EThingWith l qn cns -> return $
      case Global.lookupType qn tbl of
        Global.Error err ->
          (scopeError err exp, mempty)
        Global.SymbolFound i ->
          let
            (cns', subs) =
              resolveCNames
                (concat (Map.elems tbl))
                (symbolName i)
                (\cn -> ENotInScope (UnQual (ann cn) (unCName cn))) -- FIXME better error
                cns
            s = [i] <> subs
          in
            ( EThingWith (Scoped (Export s) l) (Scoped (GlobalSymbol i (sQName qn)) <$> qn) cns'
            , s
            )
        Global.Special {} -> error "Global.Special in export list?"
    -- FIXME ambiguity check
    EModuleContents _ modulename -> return (Scoped (Export exportedSymbols) <$> exp,exportedSymbols) where

        exportedSymbols = Set.toList (Set.intersection inScopeQualified inScopeUnqualified)

        inScopeQualified = Set.fromList (do
            (UnAnn.Qual prefix _, symbols) <- Map.toList tbl
            guard (prefix == (sModuleName modulename))
            symbols)

        inScopeUnqualified = Set.fromList (do
            (UnAnn.UnQual _, symbols) <- Map.toList tbl
            symbols)
