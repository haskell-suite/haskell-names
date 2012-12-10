{-# LANGUAGE TypeFamilies, NoMonoLocalBinds #-}
module Language.Haskell.Modules.Exports where

import qualified Data.Map as Map
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
import Language.Haskell.Modules.SyntaxUtils
import Language.Haskell.Modules.ModuleSymbols
import qualified Language.Haskell.Modules.GlobalSymbolTable as Global

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
        Left err ->
          ((\l -> ScopeError l err) <$> exp, mempty)
        Right i ->
          let s = mkVal i
          in ((\l -> Export l s) <$> exp, s)
    EAbs _ qn -> return $
      case Global.lookupType qn tbl of
        Left err ->
          ((\l -> ScopeError l err) <$> exp, mempty)
        Right i ->
          let s = mkTy i
          in ((\l -> Export l s) <$> exp, s)
    -- FIXME: the rest

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
