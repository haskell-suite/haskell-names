{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.Modules.Recursive
  ( computeInterfaces
  , getInterfaces
  , annotateModule
  ) where

import Data.Graph(stronglyConnComp, flattenSCC)
import Data.Monoid
import Data.Data (Data)
import Control.Monad
import Control.Applicative
import Language.Haskell.Exts.Annotated
import Distribution.HaskellSuite.Modules
import Data.Maybe

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
-- the results to the cache
findFixPoint
  :: (Eq l, Data l, MonadModule m, ModuleInfo m ~ Symbols)
  => [Module l] -> m ()
findFixPoint mods = go mods (map (const mempty) mods) where
  go mods syms = do
    forM_ (zip syms mods) $ \(s,m) -> insertInCache (getModuleName m) s
    syms' <- forM mods $ \m -> do
      (imp, impTbl) <- processImports $ getImports m
      let ownTbl = moduleTable m
          tbl = impTbl <> ownTbl
      (exp, syms) <- processExports tbl m
      return syms
    if syms' == syms
      then return ()
      else go mods syms'

-- | 'computeInterfaces' takes a list of possibly recursive modules and
-- computes the interface of each module. The computed interfaces are
-- written into the @m@'s cache and are available to further computations
-- in this monad.
computeInterfaces
  :: (MonadModule m, ModuleInfo m ~ Symbols, Data l, SrcInfo l, Eq l)
  => [Module l] -> m ()
computeInterfaces =
  mapM_ findFixPoint . groupModules

-- | Like 'computeInterfaces', but also return a list of interfaces, one
-- per module and in the same order
getInterfaces
  :: (MonadModule m, ModuleInfo m ~ Symbols, Data l, SrcInfo l, Eq l)
  => [Module l] -> m [Symbols]
getInterfaces mods = do
  computeInterfaces mods
  forM mods $ \mod ->
    let modName = getModuleName mod in
    fromMaybe (error $ msg modName) `liftM` lookupInCache modName
  where
    msg modName = "getInterfaces: module " ++ modToString modName ++ " is not in the cache"
