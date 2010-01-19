module Language.Haskell.Modules.Resolve(resolveModuleSource, resolveModuleName) where
import Language.Haskell.Exts.Annotated

resolveModuleName :: (SrcLoc l, MonadModule m) => ModuleName l -> m ([Msg], [Module SrcSpan])
resolveModuleName name = do
    m <- getModule name
    case m of
    ModuleSource{}   -> resolveModuleSource (m_name m) (m_contents m)
    ModuleAbbrev{}   -> return $ ([Msg MsgError noSrcLoc "Only an abbreviated module found" [MsgArg name]], [])
    ModuleNotFound{} -> return $ ([Msg MsgError noSrcLoc (m_error m) [MsgArg name], [])


resolveModuleSource :: (MonadModule m) => String -> String -> m ([Msg], [Module SrcSpan])
resolveModuleSource name src = undefined
