module Language.Haskell.Modules.Imports where

import qualified Data.Set as Set
import Control.Applicative
import Language.Haskell.Exts.Annotated
import Language.Haskell.Modules.Types
import Language.Haskell.Modules.SyntaxUtils

-- NB: this can be made more efficient
resolveImportSpec
  :: ModuleName l
  -> Bool
  -> Symbols OrigName
  -> ImportSpec l
  -> ImportSpec (Scoped l)
resolveImportSpec mod isHiding (vs,ts) spec =
  case spec of
    IVar _ n ->
      let
        matches = 
          -- Strictly speaking, the isConstructor check is unnecessary
          -- because constructors are lexically different from anything
          -- else.
          [info | info <- vs, not (isConstructor info), sv_origName info ~~ n]
      in
        checkUnique
          (ENotExported Nothing n mod)
          (matches, [])
          matches
          spec
    -- FIXME think about data families etc.
    IAbs _ n ->
      let
        matches = [info | info <- ts, st_origName info ~~ n]
      in
        checkUnique
          (ENotExported Nothing n mod)
          ([], matches)
          matches
          spec

    -- FIXME
    -- What about things like:
    -- head(..)
    -- String(..)
    -- ?
    IThingAll l n ->
      let
        matches = [info | info <- ts, st_origName info ~~ n]
        subs = [ info
               | n <- matches
               , info <- vs
               , Just n' <- return $ sv_parent info
               , n' == st_origName n ]
        n' =
          checkUnique
            (ENotExported Nothing n mod)
            ([], matches)
            matches
            n
        in
          case ann n' of
            e@ScopeError{} -> IThingAll e n'
            _ -> IThingAll (Import l (subs, matches)) n'

    IThingWith l n cns ->
      let
        matches = [info | info <- ts, st_origName info ~~ n]
        subs = [ info
               | n <- matches
               , info <- vs
               , Just n' <- return $ sv_parent info
               , n' == st_origName n ]
        n' =
          checkUnique
            (ENotExported Nothing n mod)
            ([], matches)
            matches
            n
        typeName = st_origName $ head matches -- should be safe
        cns' = map (resolveCName mod (n,typeName) (vs,ts)) cns
        ann2err :: Annotated a => a (Scoped l) -> Either (Error l) ()
        ann2err a = case ann a of ScopeError _ e -> Left e; _ -> return ()
      in
        case () of
          _ | Left e <- ann2err n' -> scopeError e spec
            | Left e <- mapM_ ann2err cns' ->
                IThingWith (ScopeError l e) n' cns'
          _ -> IThingWith (Import l (subs, matches)) n' cns'
  where
    (~~) :: GName -> Name l -> Bool
    GName _ n ~~ n' = n == nameToString n'

    isConstructor :: SymValueInfo n -> Bool
    isConstructor SymConstructor {} = True
    isConstructor _ = False

resolveCName
  :: ModuleName l
  -> (Name l, OrigName)
  -> Symbols OrigName
  -> CName l
  -> CName (Scoped l)
resolveCName mod (pname, parent) (vs, ts) cn =
  let
    matches =
      [ info
      | info <- vs
      , let GName _ name = sv_origName info
      , nameToString (unCName cn) == name
      , Just p <- return $ sv_parent info
      , p == parent
      ]
  in
    checkUnique
      (ENotExported (Just pname) (unCName cn) mod)
      (matches, [])
      matches
      cn

scopeError :: Functor f => Error l -> f l -> f (Scoped l)
scopeError e f = (\l -> ScopeError l e) <$> f

checkUnique
  :: Functor f => 
  Error l ->
  Symbols OrigName ->
  [a] ->
  f l ->
  f (Scoped l)
checkUnique notFound _ [] f = scopeError notFound f
checkUnique _ syms [_] f = fmap (\l -> Import l syms) f
-- there should be no clashes, and it should be checked elsewhere
checkUnique _ _ _ f = scopeError (EInternal "ambiguous import") f
