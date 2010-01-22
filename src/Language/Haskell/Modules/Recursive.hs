module Language.Haskell.Modules.Recursive(groupModules) where
import Data.Graph(stronglyConnComp, flattenSCC)
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.ResolveMonad(ModuleSet)
import Language.Haskell.Modules.SyntaxUtils(dropAnn, getModuleName, getImports)

-- | Take a set of modules and return a list of sets, where each sets for
-- a strongly connected component in the import graph.
-- Do not take imports using @SOURCE@ into account.
groupModules :: ModuleSet -> [ModuleSet]
groupModules lrs =
    let ls = [ l | Left l <- lrs ]
        rs = [ r | Right r <- lrs ]
        g = [ (m, dropAnn $ getModuleName m, map (dropAnn . importModule) $ filter (not . importSrc) $ getImports m) | m <- rs ]
        rs' = map (map Right) $ map flattenSCC $ stronglyConnComp g
        ls' = map ((:[]) . Left) ls
    in  ls' ++ rs'
