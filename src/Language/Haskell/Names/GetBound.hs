{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, UndecidableInstances, NamedFieldPuns,
             ScopedTypeVariables #-}
module Language.Haskell.Names.GetBound
  ( GetBound(..) -- FIXME don't export getBound
  ) where

import Data.Generics.Uniplate.Data
import Data.Data
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad
import Control.Applicative

import Language.Haskell.Exts.Annotated
import Language.Haskell.Names.Types
import Language.Haskell.Names.SyntaxUtils
import qualified Language.Haskell.Names.GlobalSymbolTable as Global

-- | Get bound value identifiers.

-- Minimal definition: either 'getBound' or 'getBoundCtx'.
--
-- Note that 'getBound' may be undefined for some instances, so the client
-- code should always call 'getBoundCtx'. 'getBound' is only provided for
-- a more convenient way to write instances. And since all instances are
-- defined in this module, we don't even export 'getBound'.
class GetBound a l | a -> l where
    getBound :: a -> [Name l]
    getBound = error "getBound is called directly. Call getBoundCtx instead"

    -- | 'getBoundCtx' is needed to resolve record wildcards. There we
    -- need to know which fields the given constructor has. So we pass the
    -- global table for that.
    getBoundCtx :: Global.Table -> a -> [Name l]
    -- default definition: ignore the context
    getBoundCtx _ = getBound

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

instance (Data l) => GetBound (Pat l) l where
  getBoundCtx gt p =
    [ n | p' <- universe $ transform dropExp p, n <- varp p' ]

    where

      varp (PVar _ n) = [n]
      varp (PAsPat _ n _) = [n]
      varp (PNPlusK _ n _) = [n]
      varp (PRec _ con fs) =
        [ n
        | -- (lazily) compute elided fields for the case when 'f' below is a wildcard
          let elidedFields = getElidedFields con fs
        , f <- fs
        , n <- getRecVars elidedFields f
        ]
      varp _ = []

      getElidedFields :: QName l -> [PatField l] -> Set.Set (Name ())
      getElidedFields con patfs =
        let
          givenFieldNames :: Set.Set (Name ())
          givenFieldNames =
            Set.fromList . map void . flip mapMaybe patfs $ \pf ->
              case pf of
                PFieldPat _ qn _ -> Just $ qNameToName qn
                PFieldPun _ n -> Just n
                PFieldWildcard {} -> Nothing

          -- FIXME must report error when the constructor cannot be
          -- resolved
          mbConOrigName =
            case Global.lookupValue con gt of
              Global.Result info@SymConstructor{} -> Just $ sv_origName info
              _ -> Nothing

          allValueInfos :: Set.Set (SymValueInfo OrigName)
          allValueInfos = Set.unions $ Map.elems $ Global.values gt

          ourFieldInfos :: Set.Set (SymValueInfo OrigName)
          ourFieldInfos =
            case mbConOrigName of
              Nothing -> Set.empty
              Just conOrigName ->
                flip Set.filter allValueInfos $ \v ->
                  case v of
                    SymSelector { sv_constructors }
                      | conOrigName `elem` sv_constructors -> True
                    _ -> False

          ourFieldNames :: Set.Set (Name ())
          ourFieldNames =
            Set.map
              (stringToName . (\(GName _ n) -> n) . origGName . sv_origName)
              ourFieldInfos

        in ourFieldNames `Set.difference` givenFieldNames

      -- must remove nested Exp so universe doesn't descend into them
      dropExp (PViewPat _ _ x) = x
      dropExp x = x

      getRecVars :: Set.Set (Name ()) -> PatField l -> [Name l]
      getRecVars _ PFieldPat {} = [] -- this is already found by the generic algorithm
      getRecVars _ (PFieldPun _ n) = [n]
      getRecVars elidedFields (PFieldWildcard l) = map (l <$) $ Set.toList elidedFields

getBoundSign :: Decl l -> [Name l]
getBoundSign (TypeSig _ ns _) = ns
getBoundSign _ = []
