{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- ModName (ModuleName l)
module Language.Haskell.Names.Imports
  ( processImport
  , processImports
  )
  where

import qualified Data.Set as Set
import Data.Monoid
import Data.Maybe
import Data.Either
import Data.Foldable (fold)
import Control.Applicative
import Control.Arrow
import Control.Monad.Writer
import Distribution.HaskellSuite.Modules
import qualified Language.Haskell.Exts as UnAnn (ModuleName(ModuleName))
import Language.Haskell.Exts.Annotated.Simplify (sName,sModuleName)
import Language.Haskell.Exts.Annotated (
    ModuleName(ModuleName),ImportDecl(..),KnownExtension(ImplicitPrelude),
    ann,ImportSpecList(..),ImportSpec(..),Name(..),
    Annotated)
import Language.Haskell.Names.Types
import Language.Haskell.Names.ScopeUtils
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import Language.Haskell.Names.SyntaxUtils
import Data.List ((\\))

instance ModName (ModuleName l) where
  modToString (ModuleName _ s) = s

preludeName :: String
preludeName = "Prelude"

processImports
  :: (MonadModule m, ModuleInfo m ~ [Symbol])
  => ExtensionSet
  -> [ImportDecl l]
  -> m ([ImportDecl (Scoped l)], Global.Table)
processImports exts importDecls = do

  (annotated,tables) <- mapM processImport importDecls >>= return . unzip
  let tbl = foldr Global.mergeTables Global.empty tables

  let
    isPreludeImported = not . null $
      [ () | ImportDecl { importModule = ModuleName _ modName } <- importDecls
           , modName == preludeName ]

    importPrelude =
      ImplicitPrelude `Set.member` exts &&
      not isPreludeImported

  tbl' <-
    if not importPrelude
      then return tbl
      else do
        -- FIXME currently we don't have a way to signal an error when
        -- Prelude cannot be found
        syms <- fold `liftM` getModuleInfo preludeName
        return $ Global.mergeTables tbl (computeSymbolTable
            False -- not qualified
            (UnAnn.ModuleName preludeName)
            syms)

  return (annotated, tbl')

processImport
  :: (MonadModule m, ModuleInfo m ~ [Symbol])
  => ImportDecl l
  -> m (ImportDecl (Scoped l), Global.Table)
processImport imp = do
  mbi <- getModuleInfo (importModule imp)
  case mbi of
    Nothing ->
      let e = EModNotFound (importModule imp)
      in return (scopeError e imp, Global.empty)
    Just syms -> return $ resolveImportDecl syms imp

resolveImportDecl
  :: [Symbol]
  -> ImportDecl l
  -> (ImportDecl (Scoped l), Global.Table)
resolveImportDecl syms (ImportDecl l mod qual src impSafe pkg mbAs mbSpecList) =
  let
    (mbSpecList', impSyms) =
      (fmap fst &&& maybe syms snd) $
        resolveImportSpecList mod syms <$> mbSpecList
    tbl = computeSymbolTable qual (sModuleName (fromMaybe mod mbAs)) impSyms
    info =
      case mbSpecList' of
        Just sl | Scoped (ScopeError e) _ <- ann sl ->
          ScopeError e
        _ -> Import tbl
  in
    (ImportDecl
      (Scoped info l)
      (Scoped (ImportPart syms) <$> mod)
      qual
      src
      impSafe
      pkg
      (fmap noScope mbAs)
      mbSpecList'
    , tbl)

resolveImportSpecList
  :: ModuleName l
  -> [Symbol]
  -> ImportSpecList l
  -> (ImportSpecList (Scoped l), [Symbol])
resolveImportSpecList mod allSyms (ImportSpecList l isHiding specs) =
  let specs' = map (resolveImportSpec mod isHiding allSyms) specs
      mentionedSyms = mconcat $ rights $ map ann2syms specs'
      importedSyms = computeImportedSymbols isHiding allSyms mentionedSyms
      newAnn = Scoped (ImportPart importedSyms) l
  in
    (ImportSpecList newAnn isHiding specs', importedSyms)

-- | This function takes care of the possible 'hiding' clause
computeImportedSymbols
  :: Bool
  -> [Symbol] -- ^ all symbols
  -> [Symbol] -- ^ mentioned symbols
  -> [Symbol] -- ^ imported symbols
computeImportedSymbols isHiding allSymbols mentionedSymbols =
  case isHiding of
    False -> mentionedSymbols
    True -> allSymbols \\ mentionedSymbols

resolveImportSpec
  :: ModuleName l
  -> Bool
  -> [Symbol]
  -> ImportSpec l
  -> ImportSpec (Scoped l)
-- NB: this can be made more efficient
resolveImportSpec mod isHiding symbols spec =
  case spec of
    IVar _ n ->
      let
        matches =
          -- Strictly speaking, the isConstructor check is unnecessary
          -- because constructors are lexically different from anything
          -- else.
          [ symbol
          | symbol <- symbols
          , not (isConstructor symbol)
          , symbol ~~ n]
      in
        checkUnique
          (ENotExported Nothing n mod)
          matches
          spec
    -- FIXME think about data families etc.
    IAbs _ _ n
      | isHiding ->
          -- This is a bit special. 'C' may match both types/classes and
          -- data constructors.
          -- FIXME Still check for uniqueness?
          let
            matches = [ symbol | symbol <- symbols, symbol ~~ n]
          in
            if null matches
              then
                scopeError (ENotExported Nothing n mod) spec
              else
                Scoped (ImportPart matches) <$> spec
      | otherwise ->
          let
            matches = [symbol | symbol <- symbols, symbol ~~ n, not (isConstructor symbol)]
          in
            checkUnique
              (ENotExported Nothing n mod)
              matches
              spec

    IThingAll l n ->
      let
        matches = [ symbol | symbol <- symbols, symbol ~~ n, hasSubImports symbol]
        subs = [ symbol
          | n <- matches
          , symbol <- symbols
          , Just n' <- return $ symbolParent symbol
          , n' == symbolName n ]
        n' =
          checkUnique
            (ENotExported Nothing n mod)
            matches
            n
        in
          case ann n' of
            e@(Scoped ScopeError{} _) -> IThingAll e n'
            _ ->
              IThingAll
                (Scoped
                  (ImportPart (subs <> matches))
                  l
                )
                n'

    IThingWith l n cns ->
      let
        matches = [symbol | symbol <- symbols, symbol ~~ n, hasSubImports symbol]
        n' =
          checkUnique
            (ENotExported Nothing n mod)
            matches
            n
        typeName = symbolName $ head matches -- should be safe
        (cns', cnSyms) =
          resolveCNames
            symbols
            typeName
            (\cn -> ENotExported (Just n) (unCName cn) mod)
            cns
      in
        IThingWith
          (Scoped
            (ImportPart (cnSyms <> matches))
            l
          )
          n'
          cns'
  where
    (~~) :: Symbol -> Name l -> Bool
    symbol ~~ name = symbolName symbol == sName name

    isConstructor :: Symbol -> Bool
    isConstructor Constructor {} = True
    isConstructor _ = False

    hasSubImports :: Symbol -> Bool
    hasSubImports symbol = case symbol of
        Data {} -> True
        NewType {} -> True
        DataFam {} -> True
        Class   {} -> True
        _ -> False

ann2syms :: Annotated a => a (Scoped l) -> Either (Error l) ([Symbol])
ann2syms a =
  case ann a of
    Scoped (ScopeError e) _ -> Left e
    Scoped (ImportPart syms) _ -> Right syms
    _ -> Left $ EInternal "ann2syms"

checkUnique
  :: Functor f =>
  Error l ->
  [Symbol] ->
  f l ->
  f (Scoped l)
checkUnique notFound symbols f =
  case length symbols of
    0 -> scopeError notFound f
    1 -> Scoped (ImportPart symbols) <$> f
    -- there should be no clashes, and it should be checked elsewhere
    _ -> scopeError (EInternal ("ambiguous import: " ++ show symbols)) f
