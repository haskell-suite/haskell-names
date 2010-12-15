import System.Environment

import Language.Haskell.Modules
import Language.Haskell.Exts.Annotated

main :: IO ()
main = do
    fns <- getArgs
    mapM_ test fns

test :: FilePath -> IO ()
test fn = do
    f <- readFile fn
    (msgs, mdls) <- pathFinder defaultPathOptions $ resolveModuleSource fn f
    putStrLn $ unlines $ map prMsg msgs
    let (msgs', mdls') = scopeAnalysis mdls
        msgs'' = getScopeErrors mdls'
    putStrLn $ unlines $ map prMsg msgs'
    putStrLn $ unlines $ map prMsg msgs''
    mapM_ (putStrLn . prettyPrint) mdls'
