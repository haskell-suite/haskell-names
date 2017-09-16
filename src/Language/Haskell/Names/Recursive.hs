{-# LANGUAGE TypeFamilies, ScopedTypeVariables #-}
module Language.Haskell.Names.Recursive
  ( resolve
  , annotate
  ) where

import Data.Foldable (traverse_)
import Data.Graph(stronglyConnComp, flattenSCC)
import Data.Data (Data)
import Control.Monad (forM, forM_, unless)

import qualified Data.Map as Map (insert)
import Control.Monad.State.Strict (State, execState, get, modify)
import Language.Haskell.Exts

import Language.Haskell.Names.Types
import Language.Haskell.Names.SyntaxUtils
import Language.Haskell.Names.ScopeUtils
import Language.Haskell.Names.ModuleSymbols
import Language.Haskell.Names.Exports
import Language.Haskell.Names.Imports
import Language.Haskell.Names.Open.Base
import Language.Haskell.Names.Annotated


-- | Takes a list of modules and an environment and updates the environment
-- with each of the given modules' exported symbols. The modules can appear
-- in any order and can be mutually recursive.
resolve :: (Data l, Eq l) => [Module l] -> Environment -> Environment
resolve modules environment = updatedEnvironment where
  moduleSCCs = groupModules modules
  updatedEnvironment = execState (traverse_ findFixPoint moduleSCCs) environment

-- | Take a set of modules and return a list of sets, where each sets for
-- a strongly connected component in the import graph.
groupModules :: [Module l] -> [[Module l]]
groupModules modules =
  map flattenSCC (stronglyConnComp (map moduleNode modules))

moduleNode :: Module l -> (Module l, ModuleName (), [ModuleName ()])
moduleNode modul =
  ( modul
  , dropAnn (getModuleName modul)
  , map (dropAnn . importModule) (getImports modul)
  )

-- | Compute interfaces for a set of mutually recursive modules and
-- update the environment accordingly.
findFixPoint :: (Data l, Eq l) => [Module l] -> State Environment ()
findFixPoint modules = loop (replicate (length modules) []) where
  loop modulesSymbols = do
    forM_ (zip modules modulesSymbols) (\(modul, symbols) -> do
      modify (Map.insert (dropAnn (getModuleName modul)) symbols))
    environment <- get
    modulesSymbols' <- forM modules (\modul -> do
      let globalTable = moduleTable (importTable environment modul) modul
      return (exportedSymbols globalTable modul))
    unless (modulesSymbols == modulesSymbols') (loop modulesSymbols')

-- | Annotate a module with scoping information using the given environment.
-- All imports of the given module should be in the environment.
annotate :: (Data l, Eq l, SrcInfo l) => Environment -> Module l -> Module (Scoped l)
annotate environment modul@(Module _ _ _ _ _) =
  Module l' maybeModuleHead' modulePragmas' importDecls' decls' where
    Module l maybeModuleHead modulePragmas importDecls decls = modul
    l' = none l
    maybeModuleHead' = case maybeModuleHead of
      Nothing -> Nothing
      Just (ModuleHead lh moduleName maybeWarning maybeExports) ->
        Just (ModuleHead lh' moduleName' maybeWarning' maybeExports') where
          lh'= none lh
          moduleName' = noScope moduleName
          maybeWarning' = fmap noScope maybeWarning
          maybeExports' = fmap (annotateExportSpecList globalTable) maybeExports
    modulePragmas' = fmap noScope modulePragmas
    importDecls' = annotateImportDecls moduleName environment importDecls
    decls' = map (annotateDecl (initialScope (dropAnn moduleName) globalTable)) decls
    globalTable = moduleTable (importTable environment modul) modul
    moduleName = getModuleName modul
annotate _ _ = error "annotateModule: non-standard modules are not supported"

