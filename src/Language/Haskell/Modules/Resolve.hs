module Language.Haskell.Modules.Resolve(ModuleSet, ModuleSummary(..), resolveModuleSource, resolveModuleName) where
import Control.Monad
import Control.Monad.Trans
import Language.Haskell.Exts.Annotated hiding (name, fileName)

import Language.Haskell.Modules.Error
import Language.Haskell.Modules.MonadModule
import Language.Haskell.Modules.ResolveMonad
import Language.Haskell.Modules.SyntaxUtils

resolveModuleName :: (SrcInfo l, MonadModule m) => ModuleName l -> m ([Msg], ModuleSet)
resolveModuleName name = runR $ resolveModule name

resolveModuleSource :: (MonadModule m) => String -> String -> m ([Msg], ModuleSet)
resolveModuleSource name src = runR $ resolveSource [] name src >> return ()

resolveSource :: (MonadModule m) => [Extension] -> String -> String -> R m (Maybe (Module SrcSpan))
resolveSource exts name src = do
    pm <- resParseMode
    case parseFileContentsWithMode (pm { parseFilename = name, extensions = exts ++ extensions pm }) src of
     ParseOk m -> do
        let m' = fmap srcInfoSpan m
        resAddModule m'
        mapM_ (resolveModule . importModule) $ getImports m'
        return (Just m')
     ParseFailed l str -> do
        resMsg $ msgError l str []
        return Nothing

resolveModule :: (SrcInfo l, MonadModule m) => ModuleName l -> R m ()
resolveModule mname = do
    b <- resSeenModule mname
    unless b $ do
        resAddModuleName mname
        mc <- lift $ getModule mname
        case mc of
            ModuleSource exts fileName src -> do
                mm <- resolveSource exts fileName src
                case mm of
                    Nothing -> return ()
                    Just m -> do
                        let mname' = getModuleName m
                        unless (mname' =~= mname) $
                            resMsg $ msgError (ann m) "Module name does not agree with imported name" [msgArg mname', msgArgLoc mname]
            ModuleAbbrev ma           -> resAddModuleSummary ma
            ModuleNotFound msg        -> resMsg $ msgError (ann mname) msg []
            ModuleIgnore              -> resMsg $ msgInfo  (ann mname) "Ignored module " [msgArg mname]
