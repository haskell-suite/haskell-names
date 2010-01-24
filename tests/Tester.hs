import System.Environment

import Language.Haskell.Modules

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
    putStrLn $ unlines $ map prMsg msgs'
