{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards #-}
-- | A module for looking up a module in the file store in the standard way.
module Language.Haskell.Modules.MonadModuleIO(IOE, PathOptions(..), pathFinder, defaultPathOptions) where
import Control.Monad.Reader
import Data.List
import Data.List.Split
import Data.Maybe
import Language.Haskell.Exts.Annotated hiding (fileName, name)
import Language.Preprocessor.Cpphs(CpphsOptions(..), defaultCpphsOptions, runCpphs)
import System.FilePath

import Language.Haskell.Modules.MonadModule

newtype IOE a = IOE (ReaderT PathOptions IO a)
    deriving (Monad, MonadReader PathOptions, MonadIO)

-- | Options when doing simple path file lookup.
data PathOptions = PathOptions {
    pPath :: [FilePath],              -- ^directories to look in (default @[""]@)
    pSuffixes :: [String],            -- ^allowed suffixes (default @["hs", "lhs"]@)
    pIgnore :: [String],              -- ^modules to exclude from the lookup process (default @["Prelude"]@)
    pExtensions :: [Extension],       -- ^language extension always in effect (default @[]@)
    pCpphsOptions :: CpphsOptions     -- ^options to cpphs (default @defaultCpphsOptions@)
    }

defaultPathOptions :: PathOptions
defaultPathOptions = PathOptions {
    pPath = [""],
    pSuffixes = ["hs", "lhs"],
    pIgnore = ["Prelude"],
    pExtensions = [],
    pCpphsOptions = defaultCpphsOptions
    }

instance MonadModule IOE where
    getModule = getModuleIOE

pathFinder :: PathOptions -> IOE a -> IO a
pathFinder popt (IOE ioe) = runReaderT ioe popt

getModuleIOE :: ModuleName l -> IOE ModuleContents
getModuleIOE (ModuleName _ "") = return $ ModuleNotFound "Empty module name"
getModuleIOE (ModuleName _ sname) = do
    igns <- asks pIgnore
    if any (ignore sname) igns then
        return ModuleIgnore
     else do
        path <- asks pPath
        suffixes <- asks pSuffixes
        let fileName = foldr1 (</>) $ splitOn "." sname
        mnf <- liftIO $ get path suffixes fileName
        case mnf of
            Just (fileName', file) -> do exts <- asks pExtensions; file' <- cpp exts fileName' file; return $ ModuleSource exts fileName' file'
            Nothing -> return $ ModuleNotFound $ "Module " ++ sname ++ " not found."

ignore :: String -> String -> Bool
ignore name pat = name == pat || take 1 (reverse pat) == "*" && reverse (drop 1 (reverse pat)) `isPrefixOf` name

get :: [String] -> [String] -> String -> IO (Maybe (FilePath, String))
get path suffixes fileName =
    pickFirst [ let f = (dir </> fileName <.> suffix)
                in  liftM (fmap (\ s -> (f, s))) $ maybeRead f
              | dir <- path, suffix <- suffixes ]

pickFirst :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
pickFirst [] = return Nothing
pickFirst (io:ios) = do
    mx <- io
    case mx of
        Nothing -> pickFirst ios
        _       -> return mx

maybeRead :: FilePath -> IO (Maybe String)
maybeRead f = liftM Just (readFile f) `catch` \ _ -> return Nothing

cpp :: [Extension] -> FilePath -> String -> IOE String
cpp exts fileName file =
    if CPP `elem` (exts ++ fromMaybe [] (readExtensions file)) then do
        opts <- asks pCpphsOptions
        file' <- liftIO $ runCpphs opts fileName file
        let file'' = unlines . map lineLINE . lines $ file'
            lineLINE s | Just l <- stripPrefix "#line " s = "{-# LINE " ++ hack l ++ " #-}"
                       | otherwise = s
--        liftIO $ putStrLn $ "CPP " ++ fileName
--        liftIO $ writeFile (fileName ++ ".xx") file''
        return file''
    else
        return file

hack :: String -> String
hack s = if last s == '"' then map (\ c -> if c == '\\' then '/' else c) s else "1 \"foo\""
