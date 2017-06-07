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
import Language.Haskell.Names.SyntaxUtils (
  dropAnn, setAnn, nameQualification, qNameToName)
import Language.Haskell.Exts
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
          FieldWildcard l ->
            FieldWildcard (Scoped (RecExpWildcard namesRes) (sLoc l)) where
              namesRes = do
                f <- sc ^. wcNames
                let localQName = qualifyName Nothing (setAnn (sLoc l) (wcFieldName f))
                    selectorQName = qualifyName (Just (wcFieldModuleName f)) (wcFieldName f)
                Scoped info _ <- return (lookupQName localQName sc)
                Scoped (GlobalSymbol symbol _) _ <- return (lookupQName selectorQName (exprRS sc))
                return (symbol, info)
          _ -> rmap go sc a
    | Just (Refl :: PatField (Scoped l) :~: a) <- eqT
      = case a of
          PFieldWildcard l ->
            PFieldWildcard (Scoped (RecPatWildcard namesRes) (sLoc l)) where
              namesRes = do
                f <- sc ^. wcNames
                let qname = qualifyName (Just (wcFieldModuleName f)) (wcFieldName f)
                Scoped (GlobalSymbol symbol _) _ <- return (lookupQName qname (exprRS sc))
                return symbol
          _ -> rmap go sc a
    | otherwise
      = rmap go sc a


lookupQName :: QName l -> Scope -> Scoped l
lookupQName (Special l _) _ = Scoped None l
lookupQName qname scope = Scoped nameInfo (ann qname) where

  nameInfo = case getL patSynMode scope of

    Nothing -> case getL nameCtx scope of

      ReferenceV -> case Local.lookupValue qname (getL lTable scope) of
        Right srcloc -> LocalValue srcloc
        _ ->
          checkUniqueness (Global.lookupValue qname globalTable)

      ReferenceT ->
        checkUniqueness (Global.lookupType qname globalTable)

      ReferenceUT ->
        checkUniqueness (Global.lookupMethodOrAssociate qname' globalTable) where
          qname' = case qname of
            UnQual _ name -> qualifyName maybeQualification name where
              maybeQualification = maybe Nothing nameQualification (getL instClassName scope)
            _ -> qname

      ReferenceRS ->
        checkUniqueness (Global.lookupSelector qname globalTable)

      _ -> None

    Just PatSynLeftHandSide -> case getL nameCtx scope of

      ReferenceV -> ValueBinder

      ReferenceRS -> ValueBinder

      _ -> None

    Just PatSynRightHandSide -> case getL nameCtx scope of

      ReferenceV -> case Local.lookupValue qname (getL lTable scope) of
        Right srcloc -> LocalValue srcloc
        _ -> checkUniqueness (Global.lookupValue qname globalTable)
      ReferenceRS ->
        checkUniqueness (Global.lookupSelector qname globalTable)

      _ -> None


  globalTable = getL gTable scope

  checkUniqueness symbols = case symbols of
    [] -> ScopeError (ENotInScope qname)
    [symbol] -> GlobalSymbol symbol (dropAnn qname)
    _ -> ScopeError (EAmbiguous qname symbols)


lookupName :: Name l -> Scope -> Scoped l
lookupName name scope = Scoped nameInfo (ann name) where

  nameInfo = case getL patSynMode scope of

    Nothing -> case getL nameCtx scope of

      ReferenceUV ->
        disambiguateMethod maybeClassName qname (Global.lookupMethodOrAssociate qname globalTable) where
          qname = qualifyName maybeQualification name
          maybeQualification = maybe Nothing nameQualification maybeClassName
          maybeClassName = getL instClassName scope

      SignatureV ->
        checkUniqueness qname (Global.lookupValue qname globalTable) where
          qname = qualifyName (Just (getL moduName scope)) name

      BindingV -> ValueBinder

      BindingT -> TypeBinder

      _ -> None

    Just PatSynLeftHandSide -> case getL nameCtx scope of

      BindingV -> ValueBinder

      _ -> None

    Just PatSynRightHandSide -> case getL nameCtx scope of

      BindingV ->
        case Local.lookupValue (qualifyName Nothing name) (getL lTable scope) of
          Right srcloc -> LocalValue srcloc
          _ -> None

      _ -> None


  globalTable = getL gTable scope


checkUniqueness :: QName l -> [Symbol] -> NameInfo l
checkUniqueness qname symbols = case symbols of
  [] -> ScopeError (ENotInScope qname)
  [symbol] -> GlobalSymbol symbol (dropAnn qname)
  _ -> ScopeError (EAmbiguous qname symbols)


disambiguateMethod :: Maybe (QName ()) -> QName l -> [Symbol] -> NameInfo l
disambiguateMethod Nothing _ _ = ScopeError (EInternal "method in instance of unknown class")
disambiguateMethod (Just instanceClassName) qname symbols = checkUniqueness qname disambiguatedSymbols where
  disambiguatedSymbols =
    filter (\symbol -> className symbol == qNameToName instanceClassName) symbols

qualifyName :: Maybe (ModuleName ()) -> Name l -> QName l
qualifyName Nothing n = UnQual (ann n) n
qualifyName (Just (ModuleName () moduleName)) n =
  Qual (ann n) annotatedModuleName n where
    annotatedModuleName = ModuleName (ann n) moduleName

