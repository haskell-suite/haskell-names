-- This module uses the open recursion interface
-- ("Language.Haskell.Names.Open") to annotate the AST with binding
-- information.
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, ImplicitParams,
    UndecidableInstances, ScopedTypeVariables,
    TypeOperators, GADTs #-}
module Language.Haskell.Names.Annotated
  ( Scoped(..)
  , NameInfo(..)
  , annotateDecl
  ) where


import Language.Haskell.Names.Types
import Language.Haskell.Names.RecordWildcards
import Language.Haskell.Names.Open.Base
import Language.Haskell.Names.Open.Instances ()
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import qualified Language.Haskell.Names.LocalSymbolTable as Local
import Language.Haskell.Names.SyntaxUtils (dropAnn, annName,setAnn)
import Language.Haskell.Exts
import Language.Haskell.Exts.Syntax
import Data.Proxy
import Data.Lens.Light
import Data.Typeable (
  Typeable, (:~:)(Refl), eqT)
  -- in GHC 7.8 Data.Typeable exports (:~:). Be careful to avoid the clash.
import Control.Applicative


annotateDecl
  :: forall a l .
     (Resolvable (a (Scoped l)), Functor a, Typeable l)
  => Scope -> a l -> a (Scoped l)
annotateDecl sc = annotateRec (Proxy :: Proxy l) sc . fmap (Scoped None)

annotateRec
  :: forall a l .
     (Typeable l, Resolvable a)
  => Proxy l -> Scope -> a -> a
annotateRec _ sc a = go sc a where
  go :: forall a . Resolvable a => Scope -> a -> a
  go sc a
    | Just (Refl :: QName (Scoped l) :~: a) <- eqT
      = lookupQName (fmap sLoc a) sc <$ a
    | Just (Refl :: Name (Scoped l) :~: a) <- eqT
      = lookupName (fmap sLoc a) sc <$ a
    | Just (Refl :: FieldUpdate (Scoped l) :~: a) <- eqT
      = case a of
          FieldPun l qname -> FieldPun l (lookupQName (sLoc <$> qname) sc <$ qname)
          FieldWildcard l -> FieldWildcard (Scoped (RecExpWildcard namesRes) (sLoc l)) where
            namesRes = do
                f <- sc ^. wcNames
                let qname = setAnn (sLoc l) (UnQual () (annName (wcFieldName f)))
                case lookupQName qname sc of
                    Scoped info@(GlobalSymbol _ _) _ -> return (wcFieldName f,info)
                    Scoped info@(LocalValue _) _ -> return (wcFieldName f,info)
                    _ -> []
          _ -> rmap go sc a
    | Just (Refl :: PatField (Scoped l) :~: a) <- eqT
    , PFieldWildcard l <- a
      = let
            namesRes = do
                f <- sc ^. wcNames
                let qname = UnQual () (annName (wcFieldName f))
                Scoped (GlobalSymbol symbol _) _ <- return (lookupQName qname (exprV sc))
                return (symbol {symbolModule = wcFieldModuleName f})
        in PFieldWildcard (Scoped (RecPatWildcard namesRes) (sLoc l))
    | otherwise
      = rmap go sc a


lookupQName :: QName l -> Scope -> Scoped l
lookupQName (Special l _) _ = Scoped None l
lookupQName qname scope = Scoped nameInfo (ann qname) where

  nameInfo = case getL nameCtx scope of

    ReferenceV -> case Local.lookupValue qname (getL lTable scope) of
      Right srcloc -> LocalValue srcloc
      _ ->
        checkUniqueness (Global.lookupValue qname globalTable)

    ReferenceT ->
      checkUniqueness (Global.lookupType qname globalTable)

    ReferenceUT ->
      checkUniqueness (Global.lookupMethodOrAssociate qname' globalTable) where
        qname' = case qname of
          UnQual _ name -> qualifyName (getL instQual scope) name
          _ -> qname

    _ -> None

  globalTable = getL gTable scope

  checkUniqueness symbols = case symbols of
    [] -> ScopeError (ENotInScope qname)
    [symbol] -> GlobalSymbol symbol (dropAnn qname)
    _ -> ScopeError (EAmbiguous qname symbols)


lookupName :: Name l -> Scope -> Scoped l
lookupName name scope = Scoped nameInfo (ann name) where

  nameInfo = case getL nameCtx scope of

    ReferenceUV ->
      checkUniqueness qname (Global.lookupMethodOrAssociate qname globalTable) where
        qname = qualifyName (getL instQual scope) name

    SignatureV ->
      checkUniqueness qname (Global.lookupValue qname globalTable) where
        qname = qualifyName (Just (getL moduName scope)) name

    BindingV -> ValueBinder

    BindingT -> TypeBinder

    _ -> None

  globalTable = getL gTable scope

  checkUniqueness qname symbols = case symbols of
    [] -> ScopeError (ENotInScope qname)
    [symbol] -> GlobalSymbol symbol (dropAnn qname)
    _ -> ScopeError (EAmbiguous qname symbols)


qualifyName :: Maybe (ModuleName ()) -> Name l -> QName l
qualifyName Nothing n = UnQual (ann n) n
qualifyName (Just (ModuleName () moduleName)) n =
  Qual (ann n) annotatedModuleName n where
    annotatedModuleName = ModuleName (ann n) moduleName

