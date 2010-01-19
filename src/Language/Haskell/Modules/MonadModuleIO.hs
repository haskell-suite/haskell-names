{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | A module for looking up a module in the file store in the standard way.
module Language.Haskell.Modules.MonadModuleIO(IOE, withPath, withPathAndSuffixes) where
import Control.Monad.Reader
import Language.Haskell.Exts.Annotated
import System.FilePath

import Language.Haskell.Modules.MonadModule

newtype IOE a = IOE (ReaderT Env IO a)
    deriving (Monad, MonadReader Env, MonadIO)

data Env = Env {
    e_path :: [String],
    e_suffixes :: [(String, String -> String)]
    }

defaultEnv :: Env
defaultEnv = Env {
    e_path = [""],
    e_suffixes = [("hs", id)]  -- , (".lhs", unlit)
    }

instance MonadModule IOE where
    getModule = getModuleIOE

withPath :: [String] -> IOE a -> IO a
withPath path (IOE ioe) = runReaderT ioe (defaultEnv { e_path = path })

withPathAndSuffixes :: [String] -> [(String, String -> String)] -> IOE a -> IO a
withPathAndSuffixes path suffs (IOE ioe) = runReaderT ioe (defaultEnv { e_path = path, e_suffixes = suffs })

getModuleIOE :: ModuleName l -> IOE ModuleContents
getModuleIOE (ModuleName _ "") = return $ ModuleNotFound "Empty module name"
getModuleIOE (ModuleName _ name) = IOE $ do
    path <- asks e_path
    suffixes <- asks e_suffixes
    let fileName = foldr1 (</>) $ split '.' name
    mnf <- liftIO $ get path suffixes fileName
    return $ case mnf of
        Just (name, file) -> ModuleSource name file
        Nothing -> ModuleNotFound $ "Module " ++ name ++ " not found."

get :: [String] -> [(String, String -> String)] -> String -> IO (Maybe (FilePath, String))
get path suffixes fileName =
    pickFirst [ let f = (dir </> fileName <.> suffix)
                in  liftM (fmap (\ s -> (f, preProc s))) $ maybeRead f
              | dir <- path, (suffix, preProc) <- suffixes ]

--- XXX
split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = []
split x xs =
    case span (/= x) xs of
    (a, []) -> [a]
    (a, _:b) -> a : split x b

pickFirst :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
pickFirst [] = return Nothing
pickFirst (io:ios) = do
    mx <- io
    case mx of
        Nothing -> pickFirst ios
        _       -> return mx

maybeRead :: FilePath -> IO (Maybe String)
maybeRead f = liftM Just (readFile f) `catch` \ _ -> return Nothing
