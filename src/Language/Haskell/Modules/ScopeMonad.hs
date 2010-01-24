{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Modules.ScopeMonad(S, runS, scopeMsg, addSymbolList{-, addSymType, addSymValue-}) where
import Control.Monad.State.Strict
import qualified Data.Map as M
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.Error
import Language.Haskell.Modules.SymbolTable

data SState = SState {
    s_modules   :: M.Map (ModuleName ()) SymbolList,
    s_messages  :: [Msg]
    }

emptySState :: SState
emptySState = SState {
    s_modules = M.empty,
    s_messages = []
    }

newtype S a = S (State SState a)
    deriving (Monad, MonadState SState)

runS :: S a -> ([Msg], a)
runS (S m) =
    case runState m $ emptySState of
    (x, s) -> (s_messages s, x)

addSymbolList :: ModuleName () -> SymbolList -> S ()
addSymbolList m l = modify $ \ s -> s { s_modules = M.insert m l (s_modules s) }

{-
addSymType :: ModuleName l -> Name l' -> [Name l''] -> S ()
addSymType m n ps = undefined

addSymValue :: ModuleName l -> Name l' -> S ()
addSymValue m n = undefined
-}

scopeMsg :: Msg -> S ()
scopeMsg msg = modify $ \ s -> s { s_messages = msg : s_messages s }
