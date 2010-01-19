module Language.Haskell.Modules.Resolve(resolveModuleSource, resolveModuleName) where
import Control.Monad
import Control.Monad.Trans
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
resolveModuleSource name src = runR $ resolveSource name src >> return ()

resolveSource :: (MonadModule m) => String -> String -> R m (Maybe SymbolDefs)
resolveSource name src =
    case parseFileContentsWithMode (makeParseMode name) src of
    ParseOk m -> do
        let m' = fmap srcInfoSpan m
            mname = getModuleName m'
        s <- summarizeModule m'
        resAddCache mname (Just s)
        resolveParsed m'
    ParseFailed l str -> do resError l str []; return Nothing

makeParseMode :: String -> ParseMode
makeParseMode name = ParseMode {
     parseFilename = name
   , extensions = []
   , ignoreLanguagePragmas = False
   , fixities = []
   , ignoreLinePragmas = False
   }

summarizeModule :: (Monad m) => Module SrcSpan -> R m SymbolDefs
summarizeModule m = do
    return SymbolDefs

resolveParsed :: (MonadModule m) => Module SrcSpan -> R m (Maybe SymbolDefs)
resolveParsed m = do
    mapM_ resolveImport $ getImports m
    -- Symbol table has everything now.
    let m' = m -- XXX
    resAddModule m'
    return $ Just SymbolDefs

resolveImport :: (MonadModule m) => ImportDecl SrcSpan -> R m ()
resolveImport i = do
    ms <- resolveModule (importModule i)
    case ms of
        Nothing -> return ()
        Just s -> do
            s' <- filterImports i s
            resAddSymbols s'

resolveModule :: (MonadModule m) => ModuleName SrcSpan -> R m (Maybe SymbolDefs)
resolveModule mname = do
    ms <- resFindModule mname
    case ms of
        Just s -> return s
        Nothing -> do
            m <- lift $ getModule mname
            case m of
                ModuleSource fileName src -> resolveSource fileName src
                ModuleAbbrev{}            -> let s = abbrevToSymbolDefs m in do resAddCache mname (Just s); return (Just s)
                ModuleNotFound msg        -> do resError (ann mname) msg []; resAddCache mname Nothing; return Nothing

filterImports :: (Monad m) => ImportDecl SrcSpan -> SymbolDefs -> R m SymbolDefs
filterImports i s = do
    return s  -- XXX

abbrevToSymbolDefs :: ModuleContents -> SymbolDefs
abbrevToSymbolDefs _ = SymbolDefs
