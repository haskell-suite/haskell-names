{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Modules.ScopeMonad(
    S, runS, scopeMsg,
    getFlags, getSymbolTable,
    addModuleSymbols, getModuleSymbols,
    addUnqualifiedSymbols, addQualifiedSymbols
    ) where
import Control.Applicative
import Control.Monad.State.Strict
import qualified Data.Map as M
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.Error
import Language.Haskell.Modules.Flags
import Language.Haskell.Modules.SymbolTable
import Language.Haskell.Modules.SyntaxUtils(dropAnn, qNameToName)

data SState = SState {
    -- Survives the entire scope checking.
    s_flags     :: Flags,
    s_modules   :: M.Map (ModuleName ()) Symbols,
    s_messages  :: [Msg],

    -- Local for a module group.
    s_symtab    :: SymbolTable
    }

emptySState :: SState
emptySState = SState {
    s_flags = defaultFlags,
    s_modules = M.empty,
    s_messages = [],
    s_symtab = symEmpty
    }

newtype S a = S (State SState a)
    deriving (Functor, Monad, MonadState SState)

instance Applicative S where
    pure = return
    f <*> a = do f' <- f; a' <- a; return (f' a')

runS :: Flags -> S a -> ([Msg], a)
runS flags (S m) =
    case runState m $ emptySState { s_flags = flags } of
    (x, s) -> (s_messages s, x)

getFlags :: S Flags
getFlags = gets s_flags

getSymbolTable :: S SymbolTable
getSymbolTable = do
    st <- gets s_symtab
    modify $ \ s -> s { s_symtab = symEmpty }
    return st

addModuleSymbols :: ModuleName () -> Symbols -> S ()
addModuleSymbols m l = modify $ \ s -> s { s_modules = M.insert m l (s_modules s) }

getModuleSymbols :: ModuleName () -> S Symbols
getModuleSymbols m = do
    sm <- gets s_modules
    case M.lookup (dropAnn m) sm of
        Nothing -> internalError $ "getModuleSymbols: " ++ prettyPrint m
        Just s -> return s

scopeMsg :: Msg -> S ()
scopeMsg msg = modify $ \ s -> s { s_messages = msg : s_messages s }

-----------------------------------------------------------------------------

addUnqualifiedSymbols :: Symbols -> S ()
addUnqualifiedSymbols = addSyms (UnQual undefined)

addQualifiedSymbols :: ModuleName l -> Symbols -> S ()
addQualifiedSymbols m = addSyms (Qual undefined $ fmap (const undefined) m)

addSyms :: (Name SrcLoc -> QName SrcLoc) -> Symbols -> S ()
addSyms q (vs, ts) = modify $ \ s -> s { s_symtab = foldr addt (foldr addv (s_symtab s) vs) ts }
  where addt i = symTypeAdd  (q $ qNameToName $ st_origName i) i
        addv i = symValueAdd (q $ qNameToName $ sv_origName i) i
