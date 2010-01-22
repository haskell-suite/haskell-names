{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Modules.ScopeMonad(S, runS, addSymbolList) where
import Control.Monad.State.Strict
import qualified Data.Map as M
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.SymbolTable

data SState = SState {
    s_modules   :: M.Map (ModuleName ()) SymbolList
--    s_messages  :: [Msg]
    }

emptySState :: SState
emptySState = SState {
    s_modules = M.empty
--    s_messages = []
    }

newtype S a = S (State SState a)
    deriving (Monad, MonadState SState)

runS :: S a -> a
runS (S m) = evalState m $ emptySState

addSymbolList :: ModuleName () -> SymbolList -> S ()
addSymbolList m l = modify $ \ s -> s { s_modules = M.insert m l (s_modules s) }
