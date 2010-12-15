-- | Flag decoder for main, somewhat ghc compatible
module Language.Haskell.Modules.Flags(decodeArgs, mainDecode) where
import System.Environment
import System.Console.GetOpt
import Data.List.Split
import Language.Preprocessor.Cpphs(CpphsOptions(..), BoolOptions(..))

import Language.Haskell.Modules -- .MonadModuleIO

descrs :: [OptDescr (PathOptions -> PathOptions)]
descrs =
    [ Option ['X'] [] (ReqArg setLang "EXTENSION")        "language extension"
    , Option ['i'] [] (ReqArg setPath "PATH")             "module search path"
    , Option ['I'] [] (ReqArg (setCPP setCppPath) "PATH") "CPP search path"
    , Option ['D'] [] (ReqArg (setCPP setDef) "DEFINE")   "CPP definition"
    ]
  where setCPP f   s o = o { pCpphsOptions = f s (pCpphsOptions o) }
        setCppPath s o = o { includes = includes o ++ [s] }
        setDef     s o = o { defines = defines o ++ [(x,drop 1 e)] } where (x, e) = span (/= '=') s
        setLang    s o = o { pExtensions = pExtensions o ++ [readExt s] }
        setPath   "" o = o { pPath = [] }
        setPath    s o = o { pPath = pPath o ++ splitOn ":" s }

        readExt s = case reads s of [(x, "")] -> x; _ -> error $ "Bad language expetnsion " ++ s

decodeArgs :: [String] -> (PathOptions, [String], [String], [String])
decodeArgs flags = (foldr (.) id opts dflt, files, unrec, errs)
  where (opts, files, unrec, errs) = getOpt' Permute descrs flags
        dflt = defaultPathOptions { pCpphsOptions = cppDflt { boolopts = cppBoolDflt { stripC89 = True } } }
        cppDflt = pCpphsOptions defaultPathOptions
        cppBoolDflt = boolopts cppDflt

mainDecode :: (ModuleSet -> IO a) -> IO [a]
mainDecode io = do
    args <- getArgs

    let oneFile opts fileName = do
            file <- readFile fileName
            (msgs, mdls) <- pathFinder opts $ resolveModuleSource fileName file
            case msgs of
                [] -> io mdls
                _ -> do
                   putStrLn $ unlines $ map prMsg msgs
                   error "failed"

        usage = usageInfo "Usage" descrs

    case decodeArgs args of
        (opts, files, [], []) -> mapM (oneFile opts) files
        (_, _, unrec@(_:_), _) -> error $ "Unrecognized option(s) " ++ unwords unrec ++ "\n" ++ usage
        (_, _, _, errs) -> error $ "Error(s) " ++ unwords errs ++ "\n" ++ usage
