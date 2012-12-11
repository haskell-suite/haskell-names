module Language.Haskell.Modules.ScopeUtils where

import Control.Applicative
import qualified Data.Set as Set
import Data.Monoid
import Data.Lens.Common
import Language.Haskell.Modules.Types
import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Modules.GlobalSymbolTable as Global

scopeError :: Functor f => Error l -> f l -> f (Scoped l)
scopeError e f = (\l -> ScopeError l e) <$> f

none :: l -> Scoped l
none = None

binder :: l -> Scoped l
binder = Binder

noScope :: (Annotated a) => a l -> a (Scoped l)
noScope = fmap None

sv_parent :: SymValueInfo n -> Maybe n
sv_parent (SymSelector { sv_typeName = n }) = Just n
sv_parent (SymConstructor { sv_typeName = n }) = Just n
sv_parent (SymMethod { sv_className = n }) = Just n
sv_parent _ = Nothing

computeSymbolTable
  :: Bool
  -> ModuleName l
  -> Symbols
  -> Global.Table
computeSymbolTable qual (ModuleName _ mod) syms =
  Global.fromLists $
    if qual
      then renamed
      else renamed <> unqualified
  where
    vs = Set.toList $ syms^.valSyms
    ts = Set.toList $ syms^.tySyms
    renamed = renameSyms mod
    unqualified = renameSyms ""
    renameSyms mod = (map (renameV mod) vs, map (renameT mod) ts)
    renameV m v =
      let GName _ n = sv_origName v
      in (GName m n, v)
    renameT m v =
      let GName _ n = st_origName v
      in (GName mod n, v)
