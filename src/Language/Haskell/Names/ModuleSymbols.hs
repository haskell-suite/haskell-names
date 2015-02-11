{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
module Language.Haskell.Names.ModuleSymbols
  ( moduleSymbols
  , moduleTable
  , getTopDeclSymbols
  )
  where

import Data.Maybe
import Data.Data
import qualified Data.Map as Map

import qualified Language.Haskell.Exts as UnAnn (ModuleName,Name)
import Language.Haskell.Exts.Annotated hiding (NewType)
import Language.Haskell.Exts.Annotated.Simplify (sModuleName,sName)
import qualified Language.Haskell.Exts.Annotated as Syntax (DataOrNew(NewType))
import Language.Haskell.Names.Types
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import Language.Haskell.Names.SyntaxUtils
import Language.Haskell.Names.ScopeUtils
import Language.Haskell.Names.GetBound

-- | Compute module's global table. It contains both the imported entities
-- and the global entities defined in this module.
moduleTable
  :: (Eq l, Data l)
  => Global.Table -- ^ the import table for this module
  -> Module l
  -> Global.Table
moduleTable impTbl m = Global.mergeTables impTbl (computeSymbolTable
  False (sModuleName (getModuleName m)) (moduleSymbols impTbl m))

-- | Compute the symbols that are defined in the given module.
--
-- The import table is needed to resolve possible top-level record
-- wildcard bindings, such as
--
-- >A {..} = foo
moduleSymbols
  :: (Eq l, Data l)
  => Global.Table -- ^ the import table for this module
  -> Module l
  -> [Symbol]
moduleSymbols impTbl m =
  concatMap (getTopDeclSymbols impTbl $ getModuleName m) (getModuleDecls m)

getTopDeclSymbols
  :: forall l . (Eq l, Data l)
  => Global.Table -- ^ the import table for this module
  -> ModuleName l
  -> Decl l
  -> [Symbol]
getTopDeclSymbols impTbl modulename d = (case d of
    TypeDecl _ dh _ -> [declHeadSymbol Type dh]

    TypeFamDecl _ dh _ -> [declHeadSymbol TypeFam dh]

    DataDecl _ dataOrNew _ dh qualConDecls _ -> declHeadSymbol (dataOrNewCon dataOrNew) dh : infos where

        dq = getDeclHeadName dh

        infos = constructorsToInfos modulename dq (qualConDeclNames qualConDecls)

    GDataDecl _ dataOrNew _ dh _ gadtDecls _ -> declHeadSymbol (dataOrNewCon dataOrNew) dh : infos where
      -- FIXME: We shouldn't create selectors for fields with existential type variables!
        dq = getDeclHeadName dh

        cons :: [(Name l,[Name l])]
        cons = do -- list monad
          GadtDecl _ cn (fromMaybe [] -> fields) _ty <- gadtDecls
          return (cn , [f | FieldDecl _ fNames _ <- fields, f <- fNames])

        infos = constructorsToInfos modulename dq cons          

    DataFamDecl _ _ dh _ -> [declHeadSymbol DataFam dh]

    ClassDecl _ _ dh _ mds ->
      let
        ms = getBound impTbl d
        cdecls = fromMaybe [] mds
      in
          (declHeadSymbol Class dh) :
        [ declHeadSymbol TypeFam dh | ClsTyFam   _   dh _ <- cdecls ] ++
        [ declHeadSymbol DataFam dh | ClsDataFam _ _ dh _ <- cdecls ] ++
        [ Method (sModuleName modulename) (sName mn) (sName (getDeclHeadName dh)) | mn <- ms ]

    FunBind _ ms -> [ Value (sModuleName modulename) (sName vn) ] where
      vn : _ = getBound impTbl ms

    PatBind _ p _ _ -> [ Value (sModuleName modulename) (sName vn) | vn <- getBound impTbl p ]

    ForImp _ _ _ _ fn _ -> [ Value (sModuleName modulename) (sName fn)]

    DataInsDecl _ _ typ qualConDecls _ -> constructorsToInfos modulename (typeOuterName typ) (qualConDeclNames qualConDecls)

    GDataInsDecl _ _ typ _ gadtDecls _ -> constructorsToInfos modulename (typeOuterName typ) cons where
      -- FIXME: We shouldn't create selectors for fields with existential type variables!
        cons :: [(Name l,[Name l])]
        cons = do -- list monad
          GadtDecl _ cn (fromMaybe [] -> fields) _ty <- gadtDecls
          return (cn , [f | FieldDecl _ fNames _ <- fields, f <- fNames])

    _ -> [])
        where
            declHeadSymbol c dh = c (sModuleName modulename) (sName (getDeclHeadName dh))

-- | Takes a type name and a list of constructor names paired with selector names. Returns
--   all symbols i.e. constructors and selectors.
constructorsToInfos :: ModuleName l -> Name l -> [(Name l,[Name l])] -> [Symbol]
constructorsToInfos modulename typename constructors = conInfos ++ selInfos where
        conInfos = do
            (constructorname,_) <- constructors
            return (Constructor (sModuleName modulename) (sName constructorname) (sName typename))

        selectorsMap = Map.fromListWith (++) (do
            (constructorname,selectornames) <- constructors
            selectorname <- selectornames
            return (nameToString selectorname,[constructorname]))

        selInfos = do
            (_,selectornames) <- constructors
            selectorname <- selectornames
            constructornames <- maybeToList (Map.lookup (nameToString selectorname) selectorsMap)
            return (Selector (sModuleName modulename) (sName selectorname) (sName typename) (map sName constructornames))

typeOuterName :: Type l -> Name l
typeOuterName t = case t of
    TyForall _ _ _ typ -> typeOuterName typ
    TyApp _ typ _ -> typeOuterName typ
    TyCon _ qname -> qNameToName qname
    TyParen _ typ -> typeOuterName typ
    TyInfix _ _ qname _ -> qNameToName qname
    TyKind _ typ _ -> typeOuterName typ
    TyBang _ _ typ -> typeOuterName typ
    _ -> error "illegal data family in data instance"

qualConDeclNames :: [QualConDecl l] -> [(Name l,[Name l])]
qualConDeclNames qualConDecls = do
    QualConDecl _ _ _ conDecl <- qualConDecls
    case conDecl of
        ConDecl _ n _ -> return (n, [])
        InfixConDecl _ _ n _ -> return (n, [])
        RecDecl _ n fields ->
            return (n , [f | FieldDecl _ fNames _ <- fields, f <- fNames])


dataOrNewCon :: Syntax.DataOrNew l -> UnAnn.ModuleName -> UnAnn.Name -> Symbol
dataOrNewCon dataOrNew = case dataOrNew of DataType {} -> Data; Syntax.NewType {} -> NewType
