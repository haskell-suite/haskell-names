{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Language.Haskell.Names.GetBound
  ( GetBound(..)
  ) where

import Data.Generics.Uniplate.Data
import Data.Data

import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Names.GlobalSymbolTable as Global

-- | Get bound value identifiers.
class GetBound a l | a -> l where
    getBound :: a -> [Name l]

-- XXX account for shadowing?
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

instance (Data l) => GetBound (Stmt l) l where
  getBound e =
    case e of
      Generator _ pat _ -> getBound pat
      LetStmt _ bnds    -> getBound bnds
      RecStmt _ stmts   -> getBound stmts
      Qualifier {} -> []

instance (Data l) => GetBound (QualStmt l) l where
  getBound e =
    case e of
      QualStmt _ stmt -> getBound stmt
      _ -> []

getBoundSign :: Decl l -> [Name l]
getBoundSign (TypeSig _ ns _) = ns
getBoundSign _ = []
instance (Data l) => GetBound (Pat l) l where
  getBound p = [ n | p' <- universe $ transform dropExp p, n <- varp p' ]
    where
      varp (PVar _ n) = [n]
      varp (PAsPat _ n _) = [n]
      varp (PNPlusK _ n _) = [n]
      varp (PRec _ _ fs) = [ n | f <- fs, n <- getRecVars f ]
      varp _ = []
      dropExp (PViewPat _ _ x) = x  -- must remove nested Exp so universe doesn't descend into them
      dropExp x = x
      getRecVars PFieldPat {} = [] -- this is already found by the generic algorithm
      getRecVars (PFieldPun _ n) = [n]
      getRecVars PFieldWildcard {} = [] -- not supported yet

