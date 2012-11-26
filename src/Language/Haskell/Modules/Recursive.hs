module Language.Haskell.Modules.Recursive(groupModules) where
import Data.Graph(stronglyConnComp, flattenSCC)
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.SyntaxUtils(dropAnn, getModuleName, getImports)

-- | Take a set of modules and return a list of sets, where each sets for
-- a strongly connected component in the import graph.
-- The boolean determines if imports using @SOURCE@ are taken into account.
groupModules :: Bool -> [Module l] -> [[Module l]]
groupModules source rs =
    let g = [ (m, dropAnn $ getModuleName m, map (dropAnn . importModule) $ filter ((source ||) . (not . importSrc)) $ getImports m) | m <- rs ]
        rs' = map flattenSCC $ stronglyConnComp g
    in rs'
