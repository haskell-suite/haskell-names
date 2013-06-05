{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.Modules.Recursive
  ( computeInterfaces
  , getInterfaces
  , annotateModule
  ) where

import Data.Graph(stronglyConnComp, flattenSCC)
import Data.Monoid
import Data.Data (Data)
import qualified Data.Set as Set
import Control.Monad hiding (forM_)
import Control.Applicative
import Language.Haskell.Exts.Annotated
import Distribution.HaskellSuite.Modules
import Data.Maybe
import Data.Foldable

import Language.Haskell.Modules.Types
import Language.Haskell.Modules.SyntaxUtils
import Language.Haskell.Modules.ScopeUtils
import Language.Haskell.Modules.ModuleSymbols
import Language.Haskell.Modules.Exports
import Language.Haskell.Modules.Imports
import qualified Language.Haskell.Modules.GlobalSymbolTable as Global
import qualified Language.Haskell.Modules.LocalSymbolTable  as Local
import Language.Haskell.Modules.Open.Base
import Language.Haskell.Modules.Annotated

-- | Take a set of modules and return a list of sets, where each sets for
-- a strongly connected component in the import graph.
-- The boolean determines if imports using @SOURCE@ are taken into account.
groupModules :: [Module l] -> [[Module l]]
groupModules modules =
  map flattenSCC $ stronglyConnComp $ map mkNode modules
  where
    mkNode m =
      ( m
      , dropAnn $ getModuleName m
      , map (dropAnn . importModule) $ getImports m
      )

-- | Annotate a module with scoping information. This assumes that all
-- module dependencies have been resolved and cached — usually you need
-- to run 'computeInterfaces' first, unless you have one module in
-- isolation.
annotateModule
  :: (MonadModule m, ModuleInfo m ~ Symbols, Data l, SrcInfo l, Eq l)
  => Module l -> m (Module (Scoped l))
annotateModule mod@(Module lm mh os is ds) = do
  (imp, impTbl) <- processImports $ getImports mod
  let ownTbl = moduleTable mod
      tbl = impTbl <> ownTbl
  (exp, syms) <- processExports tbl mod

  let
    lm' = none lm
    os' = fmap noScope os
    is' = imp
    ds' = annotate (Scope tbl Local.empty Reference) `map` ds

    mh' = flip fmap mh $ \(ModuleHead lh n mw me) ->
      let
        lh' = none lh
        n'  = noScope n
        mw' = fmap noScope mw
        me' = exp
      in ModuleHead lh' n' mw' me'

  return $ Module lm' mh' os' is' ds'

-- | Compute interfaces for a set of mutually recursive modules and write
-- the results to the cache. Return the set of import/export errors.
findFixPoint
  :: (Ord l, Data l, MonadModule m, ModuleInfo m ~ Symbols)
  => [Module l] -> m (Set.Set (Error l))
findFixPoint mods = go mods (map (const mempty) mods) where
  go mods syms = do
    forM_ (zip syms mods) $ \(s,m) -> insertInCache (getModuleName m) s
    (syms', errors) <- liftM unzip $ forM mods $ \m -> do
      (imp, impTbl) <- processImports $ getImports m
      let ownTbl = moduleTable m
          tbl = impTbl <> ownTbl
      (exp, syms) <- processExports tbl m
      return (syms, foldMap getErrors imp <> foldMap getErrors exp)
    if syms' == syms
      then return $ mconcat errors
      else go mods syms'

-- | 'computeInterfaces' takes a list of possibly recursive modules and
-- computes the interface of each module. The computed interfaces are
-- written into the @m@'s cache and are available to further computations
-- in this monad.
--
-- Returns the set of import/export errors. Note that the interfaces are
-- registered in the cache regardless of whether there are any errors, but
-- if there are errors, the interfaces may be incomplete.
computeInterfaces
  :: (MonadModule m, ModuleInfo m ~ Symbols, Data l, SrcInfo l, Ord l)
  => [Module l] -> m (Set.Set (Error l))
computeInterfaces =
  liftM fold . mapM findFixPoint . groupModules

-- | Like 'computeInterfaces', but also returns a list of interfaces, one
-- per module and in the same order
getInterfaces
  :: (MonadModule m, ModuleInfo m ~ Symbols, Data l, SrcInfo l, Ord l)
  => [Module l] -> m ([Symbols], Set.Set (Error l))
getInterfaces mods = do
  errs <- computeInterfaces mods
  ifaces <- forM mods $ \mod ->
    let modName = getModuleName mod in
    fromMaybe (error $ msg modName) `liftM` lookupInCache modName
  return (ifaces, errs)
  where
    msg modName = "getInterfaces: module " ++ modToString modName ++ " is not in the cache"
