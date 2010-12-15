import Control.Monad
import System.Environment

import Language.Haskell.Modules
import Language.Haskell.Modules.Flags
--import Language.Haskell.Modules.Options
import Language.Haskell.Exts.Annotated

main :: IO ()
main = do
    fns <- getArgs
    mapM_ test fns

test :: FilePath -> IO ()
test fn = do
    f <- readFile fn
    (msgs, mdls) <- pathFinder defaultOptions $ resolveModuleSource fn f
    putStrLn $ unlines $ map prMsg msgs
    when (null msgs) $ do
        let flags = defaultFlags { f_usePrelude = False }
        let (msgs', mdls') = scopeAnalysis flags mdls
            msgs'' = getScopeErrors mdls'
        putStrLn $ unlines $ map prMsg msgs'
        putStrLn $ unlines $ map prMsg msgs''
--        mapM_ (putStrLn . prettyPrint) mdls'
