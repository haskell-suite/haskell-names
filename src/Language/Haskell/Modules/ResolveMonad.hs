{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Modules.ResolveMonad(
    ModuleSet,
    R, runR, resError, resAddModule, resAddModuleSummary, resAddModuleName, resSeenModule,
    ) where
import Control.Monad.State.Strict
import qualified Data.Set as S
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.Error
import Language.Haskell.Modules.MonadModule
import Language.Haskell.Modules.SyntaxUtils

type ModuleSet = [Either ModuleSummary (Module SrcSpan)]

data RState = RState {
    r_seen     :: S.Set (ModuleName ()),
    r_messages :: [Msg],
    r_modules  :: ModuleSet
    }

emptyRState :: RState
emptyRState = RState {
    r_seen = S.empty,
    r_modules = [],
    r_messages = []
    }

newtype R m a = R (StateT RState m a)
    deriving (Monad, MonadState RState, MonadTrans)

runR :: (Monad m) => R m () -> m ([Msg], ModuleSet)
runR (R m) = do
    r <- execStateT m emptyRState
    return (reverse $ r_messages r, reverse $ r_modules r)

resError :: (Monad m, SrcInfo l) => l -> String -> [MsgArg] -> R m ()
resError l msg args = modify $ \ r -> r { r_messages = msgError l msg args : r_messages r }

resAddModule :: (Monad m) => Module SrcSpan -> R m ()
resAddModule m = modify $ \ r -> r { r_modules = Right m : r_modules r }

resAddModuleSummary :: (Monad m) => ModuleSummary -> R m ()
resAddModuleSummary m = modify $ \ r -> r { r_modules = Left m : r_modules r }

resAddModuleName :: (Monad m) => ModuleName l -> R m ()
resAddModuleName n = modify $ \ r -> r { r_seen = S.insert (dropAnn n) (r_seen r) }

resSeenModule :: (Monad m) => ModuleName l -> R m Bool
resSeenModule mname = liftM (S.member $ dropAnn mname) $ gets r_seen

