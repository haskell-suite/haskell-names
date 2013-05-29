module Language.Haskell.Modules.ScopeUtils where

import Control.Applicative
import Control.Arrow
import qualified Data.Set as Set
import Data.Monoid
import Data.Lens.Common
import Language.Haskell.Modules.Types
import Language.Haskell.Modules.SyntaxUtils
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
    renameSyms mod = (map (rename mod) vs, map (rename mod) ts)
    rename :: HasOrigName i => ModuleNameS -> i GName -> (GName, i GName)
    rename m v =
      let GName pkg _ n = origName v
      in (GName pkg m n, v)

resolveCName
  :: Symbols
  -> OrigName
  -> (CName l -> Error l) -- ^ error for "not found" condition
  -> CName l
  -> (CName (Scoped l), Symbols)
resolveCName syms parent notFound cn =
  let
    vs =
      [ info
      | info <- Set.toList $ syms^.valSyms
      , let GName _ _ name = sv_origName info
      , nameToString (unCName cn) == name
      , Just p <- return $ sv_parent info
      , p == parent
      ]
  in
    case vs of
      [] -> (scopeError (notFound cn) cn, mempty)
      [i] -> ((\l -> GlobalValue l i) <$> cn, mkVal i)
      _ -> (scopeError (EInternal "resolveCName") cn, mempty)

resolveCNames
  :: Symbols
  -> OrigName
  -> (CName l -> Error l) -- ^ error for "not found" condition
  -> [CName l]
  -> ([CName (Scoped l)], Symbols)
resolveCNames syms orig notFound =
  second mconcat . unzip . map (resolveCName syms orig notFound)
