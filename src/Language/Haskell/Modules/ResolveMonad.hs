{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Modules.ResolveMonad(R, runR) where
import Control.Monad.State.Strict
import qualified Data.Map as M
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.MonadModule

data RState = RState {
    r_cache :: M.Map (ModuleName ()) ModuleContents
    }

emptyRState :: RState
emptyRState = RState {
    r_cache = M.empty
    }

newtype R m a = R (StateT RState m a)
    deriving (Monad, MonadState RState, MonadTrans)

runR :: (Monad m) => R m a -> m a
runR (R m) = evalStateT m emptyRState
