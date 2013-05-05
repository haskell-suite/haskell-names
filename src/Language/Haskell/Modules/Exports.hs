{-# LANGUAGE TypeFamilies, NoMonoLocalBinds #-}
module Language.Haskell.Modules.Exports where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Data
import Data.Lens.Common
import Data.Foldable as F
import Distribution.HaskellSuite.Helpers
import Language.Haskell.Exts.Annotated
import Language.Haskell.Modules.Types
import Language.Haskell.Modules.ScopeUtils
import Language.Haskell.Modules.SyntaxUtils
import Language.Haskell.Modules.ModuleSymbols
import Language.Haskell.Modules.GlobalSymbolTable as Global

processExports
  :: (MonadModule m, ModuleInfo m ~ Symbols, Data l, Eq l)
  => Global.Table
  -> Module l
  -> m (Maybe (ExportSpecList (Scoped l)), Symbols)
processExports tbl m =
  case getExportSpecList m of
    Nothing ->
      return (Nothing, moduleSymbols m)
    Just exp ->
      liftM (first Just) $ resolveExportSpecList tbl exp

resolveExportSpecList
  :: (MonadModule m, ModuleInfo m ~ Symbols)
  => Global.Table
  -> ExportSpecList l
  -> m (ExportSpecList (Scoped l), Symbols)
resolveExportSpecList tbl (ExportSpecList l specs) =
  liftM (first (ExportSpecList $ None l)) $
  runWriterT $
  mapM (WriterT . resolveExportSpec tbl) specs

resolveExportSpec
  :: (MonadModule m, ModuleInfo m ~ Symbols)
  => Global.Table
  -> ExportSpec l
  -> m (ExportSpec (Scoped l), Symbols)
resolveExportSpec tbl exp =
  case exp of
    EVar _ qn -> return $
      case Global.lookupValue qn tbl of
        Global.Error err ->
          (scopeError err exp, mempty)
        Global.Result i ->
          let s = mkVal i
          in ((\l -> Export l s) <$> exp, s)
        Global.Special {} -> error "Global.Special in export list?"
    EAbs _ qn -> return $
      case Global.lookupType qn tbl of
        Global.Error err ->
          (scopeError err exp, mempty)
        Global.Result i ->
          let s = mkTy i
          in ((\l -> Export l s) <$> exp, s)
    EThingAll l qn -> return $
      case Global.lookupType qn tbl of
        Global.Error err ->
          (scopeError err exp, mempty)
        Global.Result i ->
          let
            subs = mconcat
              [ mkVal info
              | info <- allValueInfos
              , Just n' <- return $ sv_parent info
              , n' == st_origName i ]
            s = mkTy i <> subs
          in
            ( EThingAll (Export l s) ((\l -> GlobalType l i) <$> qn)
            , s
            )
        Global.Special {} -> error "Global.Special in export list?"
    EThingWith l qn cns -> return $
      case Global.lookupType qn tbl of
        Global.Error err ->
          (scopeError err exp, mempty)
        Global.Result i ->
          let
            (cns', subs) =
              resolveCNames
                (Global.toSymbols tbl)
                (st_origName i)
                (\cn -> ENotInScope (UnQual (ann cn) (unCName cn))) -- FIXME better error
                cns
            s = mkTy i <> subs
          in
            ( EThingWith (Export l s) ((\l -> GlobalType l i) <$> qn) cns'
            , s
            )
        Global.Special {} -> error "Global.Special in export list?"
    EModuleContents _ (ModuleName _ mod) ->
      -- FIXME ambiguity check
      let
        filterByPrefix
          :: Ord i
          => ModuleNameS
          -> Map.Map GName (Set.Set i)
          -> Set.Set i
        filterByPrefix prefix m =
          Set.unions
            [ i | (GName prefix' _, i) <- Map.toList m, prefix' == prefix ]

        filterEntities
          :: Ord i
          => Map.Map GName (Set.Set i)
          -> Set.Set i
        filterEntities ents =
          Set.intersection
            (filterByPrefix mod ents)
            (filterByPrefix ""  ents)

        eVals = filterEntities $ Global.values tbl
        eTyps = filterEntities $ Global.types tbl

        s = Symbols eVals eTyps
      in
        return ((\l -> Export l s) <$> exp, s)
  where
    allValueInfos =
      Set.toList $ Map.foldl' Set.union Set.empty $ Global.values tbl

-- Used to detect conflicts
type SymbolSet a = (Map.Map NameS [a], Map.Map NameS [a])

type Accum a = State (SymbolSet a)

addSymbols :: a -> Symbols -> Accum a ()
addSymbols a syms = do
    F.mapM_ (add fstLens) $ syms^.valSyms
    F.mapM_ (add sndLens) $ syms^.tySyms
  where
    add lens i = do
      let GName _ n = origName i
      modify $ modL lens (Map.insertWith (++) n [a])
