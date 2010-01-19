{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Modules.ResolveMonad(
    R, runR, resAddCache, resAddModule, resError,
    ModuleSummary(..)
    ) where
import Control.Monad.State.Strict
import qualified Data.Map as M
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.Error
import Language.Haskell.Modules.MonadModule
import Language.Haskell.Modules.SyntaxUtils

data RState = RState {
    r_cache    :: M.Map (ModuleName ()) ModuleSummary,
    r_messages :: [Msg],
    r_modules  :: [Module SrcSpan]
    }

emptyRState :: RState
emptyRState = RState {
    r_cache = M.empty,
    r_modules = [],
    r_messages = []
    }

newtype R m a = R (StateT RState m a)
    deriving (Monad, MonadState RState, MonadTrans)

runR :: (Monad m) => R m () -> m ([Msg], [Module SrcSpan])
runR (R m) = do
    r <- execStateT m emptyRState
    return (reverse $ r_messages r, reverse $ r_modules r)

resAddCache :: (Monad m) => ModuleName l -> ModuleSummary -> R m ()
resAddCache mname s = modify $ \ r -> r { r_cache = M.insert (dropAnn mname) s (r_cache r) }

resAddModule :: (Monad m) => Module SrcSpan -> R m ()
resAddModule m = modify $ \ r -> r { r_modules = m : r_modules r }

resError :: (Monad m, SrcInfo l) => l -> String -> [MsgArg] -> R m ()
resError l msg args = modify $ \ r -> r { r_messages = msgError l msg args : r_messages r }

-----------------------------------------------------------------------------

data ModuleSummary = ModuleSummary
