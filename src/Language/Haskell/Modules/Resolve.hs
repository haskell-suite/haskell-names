module Language.Haskell.Modules.Resolve(resolveModuleSource, resolveModuleName) where
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.Error
import Language.Haskell.Modules.MonadModule
import Language.Haskell.Modules.ResolveMonad

resolveModuleName :: (SrcInfo l, MonadModule m) => ModuleName l -> m ([Msg], [Module SrcSpan])
resolveModuleName name = do
    m <- getModule name
    case m of
        ModuleSource{}   -> resolveModuleSource (m_name m) (m_contents m)
        ModuleAbbrev{}   -> return $ ([msgError (ann name) "Only an abbreviated module found" [MsgArg name]], [])
        ModuleNotFound{} -> return $ ([msgError (ann name) (m_error m) [MsgArg name]], [])


resolveModuleSource :: (MonadModule m) => String -> String -> m ([Msg], [Module SrcSpan])
resolveModuleSource name src = runR $ resolve name src

resolve :: (MonadModule m) => String -> String -> R m a
resolve name src = undefined
