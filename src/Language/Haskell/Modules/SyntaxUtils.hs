{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Language.Haskell.Modules.SyntaxUtils(
    dropAnn, setAnn, getModuleName, getImports, getExportSpecList, splitDeclHead, getDeclHeadName, getModuleDecls,
    isTypeDecl, GetBound(..), opName, isCon, nameToString, specialConToString,
    qNameToName, unCName,
    ) where
import Data.Char
import Data.Data
import Data.Maybe
import Data.Generics.Uniplate.Data
import Language.Haskell.Exts.Annotated

dropAnn :: (Functor a) => a l -> a ()
dropAnn = fmap (const ())

setAnn :: (Functor a) => l' -> a l -> a l'
setAnn l = fmap (const l)

getModuleName :: Module l -> ModuleName l
getModuleName (Module _ (Just (ModuleHead _ mn _ _)) _ _ _) = mn
getModuleName (XmlPage _ mn _ _ _ _ _) = mn
getModuleName (XmlHybrid _ (Just (ModuleHead _ mn _ _)) _ _ _ _ _ _ _) = mn
getModuleName m = main_mod (ann m)

getImports :: Module l -> [ImportDecl l]
getImports (Module _ _ _ is _) = is
getImports (XmlPage _ _ _ _ _ _ _) = []
getImports (XmlHybrid _ _ _ is _ _ _ _ _) = is

getModuleDecls :: Module l -> [Decl l]
getModuleDecls (Module _ _ _ _ ds) = ds
getModuleDecls (XmlPage _ _ _ _ _ _ _) = []
getModuleDecls (XmlHybrid _ _ _ _ ds _ _ _ _) = ds

getExportSpecList :: Module l -> Maybe (ExportSpecList l)
getExportSpecList m = me where ModuleHead _ _ _ me = getModuleHead m

getModuleHead :: Module l -> ModuleHead l
getModuleHead (Module _ (Just mh) _ _ _) = mh
getModuleHead (XmlHybrid _ (Just mh) _ _ _ _ _ _ _) = mh
getModuleHead m = ModuleHead l (main_mod l) Nothing (Just (ExportSpecList l [EVar l (UnQual l (Ident l "main"))]))
  where l = ann m

qNameToName :: QName l -> Name l
qNameToName (UnQual _ n) = n
qNameToName (Qual _ _ n) = n
qNameToName (Special l s) = Ident l (specialConToString s)

{-
getImportDecls :: Module l -> [ImportDecl l]
getImportDecls (Module _ _ _ is _) = is
getImportDecls (XmlPage _ _ _ _ _ _ _) = []
getImportDecls (XmlHybrid _ _ _ is _ _ _ _ _) = is
-}

getDeclHead :: Decl l -> Maybe (DeclHead l)
getDeclHead (TypeDecl _ dhead _) = Just dhead
getDeclHead (TypeFamDecl _ dhead _) = Just dhead
getDeclHead (DataDecl _ _ _ dhead _ _) = Just dhead
getDeclHead (GDataDecl _ _ _ dhead _ _ _) = Just dhead
getDeclHead (DataFamDecl _ _ dhead _) = Just dhead
getDeclHead (ClassDecl _ _ dhead _ _) = Just dhead
getDeclHead _ = Nothing

splitDeclHead :: DeclHead l -> (Name l, [TyVarBind l])
splitDeclHead (DHead _ n vs) = (n, vs)
splitDeclHead (DHInfix _ v1 n v2) = (n, [v1, v2])
splitDeclHead (DHParen _ dhead) = splitDeclHead dhead

getDeclHeadName :: Decl l -> Name l
getDeclHeadName = fst . splitDeclHead . fromMaybe (error "getDeclHeadName") . getDeclHead

----------------------------------------------------

-- Get bound value identifiers.
class GetBound a l | a -> l where
    getBound :: a -> [Name l]

instance (GetBound a l) => GetBound [a] l where
    getBound xs = concatMap getBound xs

instance (GetBound a l) => GetBound (Maybe a) l where
    getBound Nothing = []
    getBound (Just x) = getBound x

instance (GetBound a l, GetBound b l) => GetBound (a, b) l where
    getBound (a, b) = getBound a ++ getBound b

instance (Data l) => GetBound (Binds l) l where
    getBound (BDecls _ ds) = getBound ds
    getBound (IPBinds _ _) = []  -- XXX doesn't bind regular identifiers

instance (Data l) => GetBound (Decl l) l where
    getBound (TypeDecl{}) = []
    getBound (TypeFamDecl{}) = []
    getBound (DataDecl _ _ _ _ ds _) = getBound ds
    getBound (GDataDecl _ _ _ _ _ ds _) = getBound ds
    getBound (DataFamDecl{}) = []
    getBound (TypeInsDecl{}) = []
    getBound (DataInsDecl _ _ _ ds _) = getBound ds
    getBound (GDataInsDecl _ _ _ _ ds _) = getBound ds
    getBound (ClassDecl _ _ _ _ mds) = getBound mds
    getBound (InstDecl{}) = []
    getBound (DerivDecl{}) = []
    getBound (InfixDecl{}) = []
    getBound (DefaultDecl{}) = []
    getBound (SpliceDecl{}) = []
    getBound (TypeSig{}) = []
    getBound (FunBind _ []) = error "getBound: FunBind []"
    getBound (FunBind _ (Match _ n _ _ _ : _)) = [n]
    getBound (FunBind _ (InfixMatch _ _ n _ _ _ : _)) = [n]
    getBound (PatBind _ p _ _ _) = getBound p
    getBound (ForImp _ _ _ _ n _) = [n]
    getBound (ForExp _ _ _ n _) = [n]
    getBound (RulePragmaDecl{}) = []
    getBound (DeprPragmaDecl{}) = []
    getBound (WarnPragmaDecl{}) = []
    getBound (InlineSig{}) = []
    getBound (SpecSig{}) = []
    getBound (SpecInlineSig{}) = []
    getBound (InstSig{}) = []
    getBound (AnnPragma{}) = []
    getBound (InlineConlikeSig{}) = []

instance (Data l) => GetBound (QualConDecl l) l where
    getBound (QualConDecl _ _ _ d) = getBound d

instance (Data l) => GetBound (GadtDecl l) l where
    getBound (GadtDecl _ n _) = [n]

instance (Data l) => GetBound (ConDecl l) l where
    getBound (ConDecl _ n _) = [n]
    getBound (InfixConDecl _ _ n _) = [n]
    getBound (RecDecl _ n fs) = n : getBound fs

instance (Data l) => GetBound (FieldDecl l) l where
    getBound (FieldDecl _ ns _) = ns

instance (Data l) => GetBound (ClassDecl l) l where
    getBound (ClsDecl _ d) = getBoundSign d
    getBound (ClsDataFam{}) = []
    getBound (ClsTyFam{}) = []
    getBound (ClsTyDef{}) = []

instance (Data l) => GetBound (Match l) l where
    getBound (Match _ n _ _ _) = [n]
    getBound (InfixMatch _ _ n _ _ _) = [n]

getBoundSign :: Decl l -> [Name l]
getBoundSign (TypeSig _ ns _) = ns
getBoundSign _ = []

instance (Data l) => GetBound (Pat l) l where
    getBound p = [ n | p' <- universe $ transform dropExp p, n <- varp p' ]
        where varp (PVar _ n) = [n]
              varp (PAsPat _ n _) = [n]
              varp _ = []
              dropExp (PViewPat _ _ x) = x  -- must remove nested Exp so universe doesn't descend into them
              dropExp x = x

isTypeDecl :: Decl l -> Bool
isTypeDecl (TypeDecl _ _ _) = True
isTypeDecl (TypeFamDecl _ _ _) = True
isTypeDecl (DataDecl _ _ _ _ _ _) = True
isTypeDecl (GDataDecl _ _ _ _ _ _ _) = True
isTypeDecl (DataFamDecl _ _ _ _) = True
isTypeDecl _ = False

opName :: Op l -> Name l
opName (VarOp _ n) = n
opName (ConOp _ n) = n

isCon :: Name l -> Bool
isCon (Ident _ (c:_)) = isUpper c
isCon (Symbol _ (':':_)) = True
isCon _ = False

nameToString :: Name l -> String
nameToString (Ident _ s) = s
nameToString (Symbol _ s) = s

specialConToString :: SpecialCon l -> String
specialConToString (UnitCon _)            = "()"
specialConToString (ListCon _)            = "[]"
specialConToString (FunCon _)             = "->"
specialConToString (TupleCon _ Boxed n)   = replicate (n-1) ','
specialConToString (TupleCon _ Unboxed n) = '#':replicate (n-1) ','
specialConToString (Cons _)               = ":"
specialConToString (UnboxedSingleCon _)   = "#"

unCName :: CName l -> Name l
unCName (VarName _ n) = n
unCName (ConName _ n) = n
