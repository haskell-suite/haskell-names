{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Modules.ScopeMonad(
    S, runS, scopeMsg,
    getFlags, getSymbolTable,
    addModuleSymbols, getModuleSymbols, getModules,
    addUnqualifiedSymbols, addQualifiedSymbols
    ) where
import Control.Applicative
import Control.Monad.State.Strict
import qualified Data.Map as M
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.Error
import Language.Haskell.Modules.Flags
import Language.Haskell.Modules.SymbolTable
import Language.Haskell.Modules.ModuleSummary
import Language.Haskell.Modules.SyntaxUtils(dropAnn, qNameToName)

-- This structure contains both read-only and read-write fields.
--
-- It is parameterised by the underlying monad. In real usage this monad
-- will be IO-based to allow reading modules on demand, but for testing
-- purposes it can be instantiated with a pure monad.
data SState m = SState {
    -- Survives the entire scope checking.
    s_flags     :: Flags,
    s_modules   :: M.Map (ModuleName ()) Symbols,
    s_messages  :: [Msg],
    s_getModuleInfo :: ModuleName () -> m ModuleSummary,

    -- Local for a module group.
    s_symtab    :: SymbolTable
    }

newtype S m a = S (StateT (SState m) m a)
    deriving (Functor, Applicative, Monad, MonadState (SState m))

runS
  :: Monad m
  => (ModuleName () -> m ModuleSummary)
  -> Flags -> S m a -> m ([Msg], a)
runS getModInfo flags (S m) = do
  (x, s) <- runStateT m initialState
  return (s_messages s, x)
  where
    initialState = SState {
      s_flags = flags,
      s_modules = M.empty,
      s_messages = [],
      s_symtab = symEmpty,
      s_getModuleInfo = getModInfo
    }


getFlags :: Monad m => S m Flags
getFlags = gets s_flags

getSymbolTable :: Monad m => S m SymbolTable
getSymbolTable = do
    st <- gets s_symtab
    modify $ \ s -> s { s_symtab = symEmpty }
    return st

addModuleSymbols :: Monad m => ModuleName () -> Symbols -> S m ()
addModuleSymbols m l = modify $ \ s -> s { s_modules = M.insert m l (s_modules s) }

getModuleSymbols :: Monad m => ModuleName () -> S m (Maybe Symbols)
getModuleSymbols m = do
    sm <- gets s_modules
    return $ M.lookup (dropAnn m) sm

getModules :: Monad m => S m (M.Map (ModuleName ()) Symbols)
getModules = gets s_modules

scopeMsg :: Monad m => Msg -> S m ()
scopeMsg msg = modify $ \ s -> s { s_messages = msg : s_messages s }

-----------------------------------------------------------------------------

addUnqualifiedSymbols :: Monad m => Symbols -> S m ()
addUnqualifiedSymbols = addSyms (UnQual undefined)

addQualifiedSymbols :: Monad m => ModuleName l -> Symbols -> S m ()
addQualifiedSymbols m = addSyms (Qual undefined $ fmap (const undefined) m)

addSyms :: Monad m => (Name SrcLoc -> QName SrcLoc) -> Symbols -> S m ()
addSyms q (vs, ts) = modify $ \ s -> s { s_symtab = foldr addt (foldr addv (s_symtab s) vs) ts }
  where addt i = symTypeAdd  (q $ qNameToName $ st_origName i) i
        addv i = symValueAdd (q $ qNameToName $ sv_origName i) i
