{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- ModName (ModuleName l)
module Language.Haskell.Names.Imports
  ( importTable
  , annotateImportDecls
  )
  where

import Data.Monoid
import Data.Maybe
import Data.Either

import Control.Monad (guard)
import Control.Monad.Writer
import Data.Map as Map (lookup)
import Language.Haskell.Exts (
  Module(Module), ModuleName(ModuleName), ImportDecl(..),
  ann,ImportSpecList(..),ImportSpec(..),Name(..),
  Annotated)
import Language.Haskell.Exts.Extension (
  Extension(DisableExtension), KnownExtension(ImplicitPrelude))
import Language.Haskell.Names.Types
import Language.Haskell.Names.ScopeUtils
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import Language.Haskell.Names.SyntaxUtils
import Data.List ((\\))


-- | Compute a table of symbols imported by the given module from the given
-- environment.
importTable :: Environment -> Module l -> Global.Table
importTable environment modul =
  foldr Global.mergeTables perhapsPrelude tables where

    tables = map (importDeclTable environment) importDecls
    Module _ _ _ importDecls _ = modul

    perhapsPrelude = if noImplicitPrelude
      then Global.empty
      else computeSymbolTable False preludeModuleName preludeSymbols
    noImplicitPrelude =
      DisableExtension ImplicitPrelude `elem` extensions || isPreludeImported
    isPreludeImported = not (null (do
      importDecl <- importDecls
      guard (dropAnn (importModule importDecl) == preludeModuleName)))
    preludeSymbols = fromMaybe [] (Map.lookup preludeModuleName environment)
    (_, extensions) = getModuleExtensions modul

preludeModuleName :: ModuleName ()
preludeModuleName = ModuleName () "Prelude"

importDeclTable :: Environment -> ImportDecl l -> Global.Table
importDeclTable environment importDecl =
  computeSymbolTable isQualified moduleName importSymbols where
    ImportDecl _ importModuleName isQualified _ _ _ maybeAs maybeImportSpecList =
      importDecl
    moduleName = dropAnn (fromMaybe importModuleName maybeAs)
    importSymbols = case maybeImportSpecList of
      Nothing ->
        importModuleSymbols
      Just importSpecList ->
        importSpecListSymbols importModuleName importModuleSymbols importSpecList
    importModuleSymbols = fromMaybe [] (
      Map.lookup (dropAnn importModuleName) environment)


importSpecListSymbols :: ModuleName l -> [Symbol] -> ImportSpecList l -> [Symbol]
importSpecListSymbols importModuleName allSymbols importSpecList =
  if isHiding
    then allSymbols \\ mentionedSymbols
    else mentionedSymbols where
      ImportSpecList _ isHiding importSpecs = importSpecList
      annotatedImportSpecs =
        map (resolveImportSpec importModuleName isHiding allSymbols) importSpecs
      mentionedSymbols =
        mconcat (rights (map ann2syms annotatedImportSpecs))

-- | Annotate the given list of import declarations with scoping information
-- against the given environment. We need the name of the module that contains
-- the import declarations for error annotations.
annotateImportDecls ::
  ModuleName l -> Environment -> [ImportDecl l] -> [ImportDecl (Scoped l)]
annotateImportDecls moduleName environment importDecls =
  map (annotateImportDecl moduleName environment) importDecls


annotateImportDecl ::
  ModuleName l -> Environment -> ImportDecl l -> ImportDecl (Scoped l)
annotateImportDecl moduleName environment importDecl = importDecl' where
  ImportDecl
    l
    importModuleName
    isQualified
    isSource
    isSafe
    importPackage
    maybeAs
    maybeImportSpecList = importDecl

  importDecl' = case Map.lookup (dropAnn importModuleName) environment of
    Nothing -> scopeError (EModNotFound importModuleName) importDecl
    Just symbols ->
      ImportDecl
        l'
        importModuleName'
        isQualified
        isSource
        isSafe
        importPackage
        maybeAs'
        maybeImportSpecList' where
          l' = Scoped info l
          importModuleName' = fmap (Scoped (ImportPart symbols)) importModuleName
          maybeAs' = fmap noScope maybeAs
          maybeImportSpecList' =
            fmap (annotateImportSpecList moduleName symbols) maybeImportSpecList
          info = case maybeImportSpecList' of
            Just sl | Scoped (ScopeError e) _ <- ann sl ->
              ScopeError e
            _ -> Import table
          table = computeSymbolTable isQualified qualificationName importSymbols
          qualificationName = dropAnn (fromMaybe importModuleName maybeAs)
          importSymbols =
            maybe
              symbols
              (importSpecListSymbols moduleName symbols)
              maybeImportSpecList


annotateImportSpecList ::
  ModuleName l -> [Symbol] -> ImportSpecList l -> ImportSpecList (Scoped l)
annotateImportSpecList moduleName allSymbols importSpecList =
  (ImportSpecList l' isHiding importSpecs') where
    ImportSpecList l isHiding importSpecs = importSpecList
    l' = Scoped (ImportPart importSymbols) l
    importSpecs' = map (resolveImportSpec moduleName isHiding allSymbols) importSpecs
    importSymbols = importSpecListSymbols moduleName allSymbols importSpecList


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
    symbol ~~ name = symbolName symbol == dropAnn name

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
