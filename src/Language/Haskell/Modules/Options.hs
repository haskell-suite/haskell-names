-- | Flag decoder for main, somewhat ghc compatible
module Language.Haskell.Modules.Options(decodeArgs, mainDecode) where
import Data.List.Split
import Data.Maybe
import System.Console.GetOpt
import System.Environment

import Language.Preprocessor.Cpphs(CpphsOptions(..), BoolOptions(..))

import Language.Haskell.Modules
import Language.Haskell.Modules.Flags

descrs :: [OptDescr (Options -> Options)]
descrs =
    [ Option ['X'] [] (ReqArg setLang "EXTENSION")        "language extension"
    , Option ['i'] [] (ReqArg setPath "PATH")             "module search path"
    , Option ['I'] [] (ReqArg (setCPP setCppPath) "PATH") "CPP search path"
    , Option ['D'] [] (ReqArg (setCPP setDef) "DEFINE")   "CPP definition"
    , Option ['f'] [] (ReqArg setFlags "FLAG")            "compilation flags"
    ]
  where setCPP f   s o = o { pCpphsOptions = f s (pCpphsOptions o) }
        setCppPath s o = o { includes = includes o ++ [s] }
        setDef     s o = o { defines = defines o ++ [(x,drop 1 e)] } where (x, e) = span (/= '=') s
        setLang    s o = o { pExtensions = pExtensions o ++ [readExt s] }
        setPath   "" o = o { pPath = [] }
        setPath    s o = o { pPath = pPath o ++ splitOn ":" s }
        setFlags   s o = o { pFlags = fromMaybe (error $ "Bad flag " ++ s) $ setFlag (pFlags o) s }

        readExt s = case reads s of [(x, "")] -> x; _ -> error $ "Bad language expetnsion " ++ s

decodeArgs :: [String] -> (Options, [String], [String], [String])
decodeArgs flags = (foldr (.) id opts dflt, files, unrec, errs)
  where (opts, files, unrec, errs) = getOpt' Permute descrs flags
        dflt = defaultOptions { pCpphsOptions = cppDflt { boolopts = cppBoolDflt { stripC89 = True } } }
        cppDflt = pCpphsOptions defaultOptions
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
