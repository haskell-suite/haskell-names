{-# LANGUAGE RelaxedPolyRec #-}
module Language.Haskell.Modules.Resolve(ModuleSet, ModuleSummary(..), resolveModuleSource, resolveModuleName) where
import Control.Monad
import Control.Monad.Trans
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.Error
import Language.Haskell.Modules.MonadModule
import Language.Haskell.Modules.ResolveMonad
import Language.Haskell.Modules.SyntaxUtils

resolveModuleName :: (SrcInfo l, MonadModule m) => ModuleName l -> m ([Msg], ModuleSet)
resolveModuleName name = runR $ resolveModule name

resolveModuleSource :: (MonadModule m) => String -> String -> m ([Msg], ModuleSet)
resolveModuleSource name src = runR $ resolveSource name src >> return ()

resolveSource :: (MonadModule m) => String -> String -> R m (Maybe (Module SrcSpan))
resolveSource name src =
    case parseFileContentsWithMode (makeParseMode name) src of
    ParseOk m -> do
        let m' = fmap srcInfoSpan m
            mname = getModuleName m'
        resAddModuleName mname
        resAddModule m'
        mapM_ (resolveModule . importModule) $ getImports m
        return (Just m')
    ParseFailed l str -> do
        resError l str []
        return Nothing

makeParseMode :: String -> ParseMode
makeParseMode name = ParseMode {
     parseFilename = name
   , extensions = []
   , ignoreLanguagePragmas = False
   , fixities = []
   , ignoreLinePragmas = False
   }

resolveModule :: (SrcInfo l, MonadModule m) => ModuleName l -> R m ()
resolveModule mname = do
    b <- resSeenModule mname
    unless b $ do
        m <- lift $ getModule mname
        case m of
            ModuleSource fileName src -> do
                mm <- resolveSource fileName src
                case mm of
                    Nothing -> return ()
                    Just m -> do
                        let mname' = getModuleName m
                        unless (mname' =~= mname) $
                            resError (ann m) "Module name does not agree with imported name" [MsgArg mname', MsgArg mname]
            ModuleAbbrev ma           -> resAddModuleSummary ma
            ModuleNotFound msg        -> do resError (ann mname) msg []; resAddModuleName mname
