module Language.Haskell.Modules.Resolve(resolveModuleSource, resolveModuleName) where
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.Error
import Language.Haskell.Modules.MonadModule
import Language.Haskell.Modules.ResolveMonad
import Language.Haskell.Modules.SyntaxUtils

resolveModuleName :: (SrcInfo l, MonadModule m) => ModuleName l -> m ([Msg], [Module SrcSpan])
resolveModuleName name = do
    m <- getModule name
    case m of
        ModuleSource{}   -> resolveModuleSource (m_name m) (m_contents m)
        ModuleAbbrev{}   -> return $ ([msgError (ann name) "Only an abbreviated module found" [MsgArg name]], [])
        ModuleNotFound{} -> return $ ([msgError (ann name) (m_error m) [MsgArg name]], [])


resolveModuleSource :: (MonadModule m) => String -> String -> m ([Msg], [Module SrcSpan])
resolveModuleSource name src = runR $ resolveSource name src

resolveSource :: (MonadModule m) => String -> String -> R m ()
resolveSource name src =
    case parseFileContentsWithMode (makeParseMode name) src of
    ParseOk m -> do
        let m' = fmap srcInfoSpan m
            mname = moduleName m'
        resAddCache mname (summarizeModule m')
        resAddModule m'
        resolveParsed m'
    ParseFailed l str -> resError l str []

makeParseMode :: String -> ParseMode
makeParseMode name = ParseMode {
     parseFilename = name
   , extensions = []
   , ignoreLanguagePragmas = False
   , fixities = []
   , ignoreLinePragmas = False
   }

summarizeModule :: Module SrcSpan -> ModuleSummary
summarizeModule m = undefined

resolveParsed :: (Monad m) => Module SrcSpan -> R m ()
resolveParsed m = undefined
