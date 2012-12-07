{-# LANGUAGE TypeFamilies #-}
module Language.Haskell.Modules.Imports
  ( processImport
  , processImports
  )
  where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Monoid hiding (First(..))
import Data.Maybe
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Writer
import Distribution.HaskellSuite.Helpers
import Language.Haskell.Exts.Annotated
import Language.Haskell.Modules.Types
import qualified Language.Haskell.Modules.GlobalSymbolTable as Global
import Language.Haskell.Modules.SyntaxUtils

instance ModName (ModuleName l) where
  modToString (ModuleName _ s) = s

newtype EitherMonoid e a = EitherMonoid { getEither :: Either e a }

instance Monoid a => Monoid (EitherMonoid e a) where
  mempty = EitherMonoid $ pure mempty
  mappend (EitherMonoid a) (EitherMonoid b) =
    EitherMonoid $ mappend <$> a <*> b

processImports
  :: (MonadModule m, ModuleInfo m ~ Symbols OrigName)
  => [ImportDecl l]
  -> m ([ImportDecl (Scoped l)], Either (Error l) Global.Table)
processImports = unwrap . mapM (wrap . processImport)
  where
    wrap = WriterT . liftM (second EitherMonoid)
    unwrap = liftM (second getEither) . runWriterT

processImport
  :: (MonadModule m, ModuleInfo m ~ Symbols OrigName)
  => ImportDecl l
  -> m (ImportDecl (Scoped l), Either (Error l) Global.Table)
processImport imp = do
  mbi <- getModuleInfo (importModule imp)
  case mbi of
    Nothing ->
      let e = EModNotFound (importModule imp)
      in return $ (scopeError e imp, Left e)
    Just syms ->
      let imp' = resolveImportDecl syms imp
      in return (imp', ann2table imp')

resolveImportDecl
  :: Symbols OrigName
  -> ImportDecl l
  -> ImportDecl (Scoped l)
resolveImportDecl syms (ImportDecl l mod qual src pkg mbAs mbSpecList) =
  let
    mbSpecList' = resolveImportSpecList mod syms <$> mbSpecList
    tbl = do
      impSyms <- maybe (return syms) ann2syms mbSpecList'
      return $ computeSymbolTable qual (fromMaybe mod mbAs) impSyms
    newAnn = either (ScopeError l) (Import l) tbl
  in
    ImportDecl
      newAnn
      ((\l -> ImportPart l syms) <$> mod)
      qual
      src
      pkg
      (fmap (None <$>) mbAs)
      mbSpecList'

computeSymbolTable
  :: Bool
  -> ModuleName l
  -> Symbols OrigName
  -> Global.Table
computeSymbolTable qual (ModuleName _ mod) (vs,ts) =
  Global.fromLists $
    if qual
      then renamed
      else renamed <> unqualified
  where
    renamed = renameSyms mod
    unqualified = renameSyms ""
    renameSyms mod = (map (renameV mod) vs, map (renameT mod) ts)
    renameV m v =
      let GName _ n = sv_origName v
      in (GName m n, v)
    renameT m v =
      let GName _ n = st_origName v
      in (GName mod n, v)

resolveImportSpecList
  :: ModuleName l
  -> Symbols OrigName
  -> ImportSpecList l
  -> ImportSpecList (Scoped l)
resolveImportSpecList mod allSyms@(vs,ts) (ImportSpecList l isHiding specs) =
  let specs' = map (resolveImportSpec mod isHiding allSyms) specs
      mentionedSyms = mconcat <$> mapM ann2syms specs'
  in
    case mentionedSyms of
      Left e -> ImportSpecList (ScopeError l e) isHiding specs'
      Right syms ->
        case isHiding of
          False -> ImportSpecList (ImportPart l syms) isHiding specs'
          True ->
            let
              (hvs, hts) = syms
              allTys = symbolMap st_origName ts
              hidTys = symbolMap st_origName hts
              allVls = symbolMap sv_origName vs
              hidVls = symbolMap sv_origName hvs
              resultSyms =
                ( Map.elems $ allVls Map.\\ hidVls
                , Map.elems $ allTys Map.\\ hidTys )
            in ImportSpecList (ImportPart l resultSyms) isHiding specs'

symbolMap
  :: Ord s
  => (a -> s)
  -> [a]
  -> Map.Map s a
symbolMap f is = Map.fromList [(f i, i) | i <- is]

resolveImportSpec
  :: ModuleName l
  -> Bool
  -> Symbols OrigName
  -> ImportSpec l
  -> ImportSpec (Scoped l)
-- NB: this can be made more efficient
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
    IAbs _ n
      | isHiding ->
          -- This is a bit special. 'C' may match both types/classes and
          -- data constructors.
          -- FIXME Still check for uniqueness?
          let
            tyMatches =
              [info | info <- ts, st_origName info ~~ n]
            vlMatches =
              [info | info <- vs, sv_origName info ~~ n]
          in
            if null tyMatches && null vlMatches
              then
                scopeError (ENotExported Nothing n mod) spec
              else
                (\l -> Export l (vlMatches, tyMatches)) <$> spec
      | otherwise ->
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
            _ -> IThingAll (ImportPart l (subs, matches)) n'

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
      in
        case () of
          _ | Left e <- ann2syms n' -> scopeError e spec
            | Left e <- mapM_ ann2syms cns' ->
                IThingWith (ScopeError l e) n' cns'
          _ -> IThingWith (ImportPart l (subs, matches)) n' cns'
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

ann2syms :: Annotated a => a (Scoped l) -> Either (Error l) (Symbols OrigName)
ann2syms a =
  case ann a of
    ScopeError _ e -> Left e
    ImportPart _ syms -> Right syms
    _ -> Left $ EInternal "ann2syms"

ann2table :: Annotated a => a (Scoped l) -> Either (Error l) Global.Table
ann2table a =
  case ann a of
    ScopeError _ e -> Left e
    Import _ tbl -> Right tbl
    _ -> Left $ EInternal "ann2syms"

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
checkUnique _ syms [_] f = fmap (\l -> ImportPart l syms) f
-- there should be no clashes, and it should be checked elsewhere
checkUnique _ _ _ f = scopeError (EInternal "ambiguous import") f
