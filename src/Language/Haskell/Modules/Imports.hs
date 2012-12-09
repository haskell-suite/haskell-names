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
import Data.Either
import Data.Lens.Common
import Data.Foldable (foldMap)
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

processImports
  :: (MonadModule m, ModuleInfo m ~ Symbols)
  => [ImportDecl l]
  -> m ([ImportDecl (Scoped l)], Global.Table)
processImports = runWriterT . mapM (WriterT . processImport)

processImport
  :: (MonadModule m, ModuleInfo m ~ Symbols)
  => ImportDecl l
  -> m (ImportDecl (Scoped l), Global.Table)
processImport imp = do
  mbi <- getModuleInfo (importModule imp)
  case mbi of
    Nothing ->
      let e = EModNotFound (importModule imp)
      in return $ (scopeError e imp, Global.empty)
    Just syms -> return $ resolveImportDecl syms imp

resolveImportDecl
  :: Symbols
  -> ImportDecl l
  -> (ImportDecl (Scoped l), Global.Table)
resolveImportDecl syms (ImportDecl l mod qual src pkg mbAs mbSpecList) =
  let
    (mbSpecList', impSyms) =
      (fmap fst &&& maybe syms snd) $
        resolveImportSpecList mod syms <$> mbSpecList
    tbl = computeSymbolTable qual (fromMaybe mod mbAs) impSyms
    newAnn =
      case mbSpecList' of
        Just sl | ScopeError _ e <- ann sl -> ScopeError l e
        _ -> Import l tbl
  in
    (ImportDecl
      newAnn
      ((\l -> ImportPart l syms) <$> mod)
      qual
      src
      pkg
      (fmap (None <$>) mbAs)
      mbSpecList'
    , tbl)

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

resolveImportSpecList
  :: ModuleName l
  -> Symbols
  -> ImportSpecList l
  -> (ImportSpecList (Scoped l), Symbols)
resolveImportSpecList mod allSyms (ImportSpecList l isHiding specs) =
  let specs' = map (resolveImportSpec mod isHiding allSyms) specs
      mentionedSyms = mconcat $ rights $ map ann2syms specs'
      importedSyms = computeImportedSymbols isHiding allSyms mentionedSyms
      newAnn = ImportPart l importedSyms
  in
    (ImportSpecList newAnn isHiding specs', importedSyms)

-- | This function takes care of the possible 'hiding' clause
computeImportedSymbols
  :: Bool
  -> Symbols -- ^ all symbols
  -> Symbols -- ^ mentioned symbols
  -> Symbols -- ^ imported symbols
computeImportedSymbols isHiding (Symbols vs ts) mentionedSyms =
  case isHiding of
    False -> mentionedSyms
    True ->
      let
        Symbols hvs hts = mentionedSyms
        allTys = symbolMap st_origName ts
        hidTys = symbolMap st_origName hts
        allVls = symbolMap sv_origName vs
        hidVls = symbolMap sv_origName hvs
      in
        Symbols
          (Set.fromList $ Map.elems $ allVls Map.\\ hidVls)
          (Set.fromList $ Map.elems $ allTys Map.\\ hidTys)

symbolMap
  :: Ord s
  => (a -> s)
  -> Set.Set a
  -> Map.Map s a
symbolMap f is = Map.fromList [(f i, i) | i <- Set.toList is]

resolveImportSpec
  :: ModuleName l
  -> Bool
  -> Symbols
  -> ImportSpec l
  -> ImportSpec (Scoped l)
-- NB: this can be made more efficient
resolveImportSpec mod isHiding syms spec =
  case spec of
    IVar _ n ->
      let
        matches = mconcat $
          -- Strictly speaking, the isConstructor check is unnecessary
          -- because constructors are lexically different from anything
          -- else.
          [ mkVal info
          | info <- vs
          , not (isConstructor info)
          , sv_origName info ~~ n]
      in
        checkUnique
          (ENotExported Nothing n mod)
          matches
          spec
    -- FIXME think about data families etc.
    IAbs _ n
      | isHiding ->
          -- This is a bit special. 'C' may match both types/classes and
          -- data constructors.
          -- FIXME Still check for uniqueness?
          let
            matches@(Symbols vlMatches tyMatches) =
              mconcat [ mkVal info | info <- vs, sv_origName info ~~ n]
              <>
              mconcat [ mkTy info | info <- ts, st_origName info ~~ n]
          in
            if Set.null tyMatches && Set.null vlMatches
              then
                scopeError (ENotExported Nothing n mod) spec
              else
                (\l -> Export l (Symbols vlMatches tyMatches)) <$> spec
      | otherwise ->
          let
            matches = mconcat
              [mkTy info | info <- ts, st_origName info ~~ n]
          in
            checkUnique
              (ENotExported Nothing n mod)
              matches
              spec

    -- FIXME
    -- What about things like:
    -- head(..)
    -- String(..)
    -- ?
    IThingAll l n ->
      let
        matches = [ info | info <- ts, st_origName info ~~ n]
        subs = mconcat
          [ mkVal info
          | n <- matches
          , info <- vs
          , Just n' <- return $ sv_parent info
          , n' == st_origName n ]
        n' =
          checkUnique
            (ENotExported Nothing n mod)
            (foldMap mkTy matches)
            n
        in
          case ann n' of
            e@ScopeError{} -> IThingAll e n'
            _ ->
              IThingAll
                (ImportPart l
                  (subs <> foldMap mkTy matches))
                n'

    IThingWith l n cns ->
      let
        matches = [info | info <- ts, st_origName info ~~ n]
        subs = mconcat
          [ mkVal info
          | n <- matches
          , info <- vs
          , Just n' <- return $ sv_parent info
          , n' == st_origName n ]
        n' =
          checkUnique
            (ENotExported Nothing n mod)
            (foldMap mkTy matches)
            n
        typeName = st_origName $ head matches -- should be safe
        cns' = map (resolveCName mod (n,typeName) syms) cns
      in
        case () of
          _ | Left e <- ann2syms n' -> scopeError e spec
            | Left e <- mapM_ ann2syms cns' ->
                IThingWith (ScopeError l e) n' cns'
          _ ->
            IThingWith
              (ImportPart l (subs <> foldMap mkTy matches))
              n'
              cns'
  where
    (~~) :: GName -> Name l -> Bool
    GName _ n ~~ n' = n == nameToString n'

    isConstructor :: SymValueInfo n -> Bool
    isConstructor SymConstructor {} = True
    isConstructor _ = False

    vs = Set.toList $ syms^.valSyms
    ts = Set.toList $ syms^.tySyms

resolveCName
  :: ModuleName l
  -> (Name l, OrigName)
  -> Symbols
  -> CName l
  -> CName (Scoped l)
resolveCName mod (pname, parent) syms cn =
  let
    matches = mconcat
      [ mkVal info
      | info <- Set.toList $ syms^.valSyms
      , let GName _ name = sv_origName info
      , nameToString (unCName cn) == name
      , Just p <- return $ sv_parent info
      , p == parent
      ]
  in
    checkUnique
      (ENotExported (Just pname) (unCName cn) mod)
      matches
      cn

ann2syms :: Annotated a => a (Scoped l) -> Either (Error l) (Symbols)
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

checkUnique
  :: Functor f =>
  Error l ->
  Symbols ->
  f l ->
  f (Scoped l)
checkUnique notFound syms@(Symbols vs ts) f =
  case Set.size vs + Set.size ts of
    0 -> scopeError notFound f
    1 -> fmap (\l -> ImportPart l syms) f
    -- there should be no clashes, and it should be checked elsewhere
    _ -> scopeError (EInternal "ambiguous import") f
