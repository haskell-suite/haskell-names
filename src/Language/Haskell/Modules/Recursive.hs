{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.Modules.Recursive(analyseModules) where

import Data.Graph(stronglyConnComp, flattenSCC)
import Data.Monoid
import Data.Data
import Control.Monad
import Control.Applicative
import Language.Haskell.Exts.Annotated
import Distribution.HaskellSuite.Helpers

import Language.Haskell.Modules.Types
import Language.Haskell.Modules.SyntaxUtils
import Language.Haskell.Modules.ScopeUtils
import Language.Haskell.Modules.ModuleSymbols
import Language.Haskell.Modules.Exports
import Language.Haskell.Modules.Imports
import Language.Haskell.Modules.ScopeCheck
import Language.Haskell.Modules.ScopeCheckMonad

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

scopeSCC
  :: (MonadModule m, ModuleInfo m ~ Symbols)
  => [Module SrcSpan] -> m [(Module (Scoped SrcSpan), Symbols)]
scopeSCC mods = do
  modData <- go $ map (const mempty) mods
  return $ flip map (zip mods modData) $
    \(Module lm mh os is ds, (imp, exp, tbl, syms)) ->
      let
        lm' = none lm
        os' = fmap noScope os
        is' = imp
        ds' = runScopeM (mapM scopeR ds) tbl

        mh' = flip fmap mh $ \(ModuleHead lh n mw me) ->
          let
            lh' = none lh
            n'  = noScope n
            mw' = fmap noScope mw
            me' = exp
          in ModuleHead lh' n' mw' me'

      in (Module lm' mh' os' is' ds', syms)

  where
    go syms = do
      forM_ (zip syms mods) $ \(s,m) -> insertInCache (getModuleName m) s
      new <- forM mods $ \m -> do
        (imp, tbl) <- processImports $ getImports m
        (exp, syms) <- processExports tbl m
        return (imp, exp, tbl, syms)
      let syms' = map (\(_,_,_,x) -> x) new
      if syms' == syms
        then return new
        else go syms'

analyseModules
  :: (MonadModule m, ModuleInfo m ~ Symbols)
  => [Module SrcSpan] -> m [(Module (Scoped SrcSpan), Symbols)]
analyseModules =
  liftM concat . mapM scopeSCC . groupModules
