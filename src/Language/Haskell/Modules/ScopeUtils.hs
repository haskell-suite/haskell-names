module Language.Haskell.Modules.ScopeUtils where

import Control.Applicative
import Language.Haskell.Modules.Types

scopeError :: Functor f => Error l -> f l -> f (Scoped l)
scopeError e f = (\l -> ScopeError l e) <$> f

sv_parent :: SymValueInfo n -> Maybe n
sv_parent (SymSelector { sv_typeName = n }) = Just n
sv_parent (SymConstructor { sv_typeName = n }) = Just n
sv_parent (SymMethod { sv_className = n }) = Just n
sv_parent _ = Nothing
