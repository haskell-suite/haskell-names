{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.Modules.ScopeCheck where

import Prelude hiding (mapM)
import Control.Applicative
import Data.List hiding (mapM)
import Data.Traversable (mapM)

import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.Types
import Language.Haskell.Modules.SyntaxUtils
import Language.Haskell.Modules.ScopeUtils
import Language.Haskell.Modules.ScopeCheckMonad

scopeX
  :: SrcInfo a
  => (SymTypeInfo OrigName -> QName a -> Maybe (Error a))
  -> QName a -> ScopeM i (QName (Scoped a))
scopeX ok qn = do
  r <- lookupType qn
  let
    f l =
      case r of
        Left e -> ScopeError l e
        Right i ->
          case ok i qn of
            Nothing -> GlobalType l i
            Just e -> ScopeError l e
  return $ fmap f qn

scopeTyCls :: QName SrcSpan -> ScopeM i (QName (Scoped SrcSpan))
scopeTyCls = scopeX (\_ _ -> Nothing)

scopeCls :: QName SrcSpan -> ScopeM i (QName (Scoped SrcSpan))
scopeCls = scopeX cls
  where cls SymClass{} _ = Nothing
        cls _ qn = Just $ ETypeAsClass qn

scopeTy :: QName SrcSpan -> ScopeM i (QName (Scoped SrcSpan))
scopeTy = scopeX cls
  where cls SymClass{} qn = Just $ EClassAsType qn
        cls _ _ = Nothing

scopeTyVar :: Name SrcSpan -> ScopeM i (Name (Scoped SrcSpan))
scopeTyVar n = return $ fmap (\l -> TypeVar l (getPointLoc l)) n -- FIXME

scopeVal :: QName SrcSpan -> ScopeM i (QName (Scoped SrcSpan))
scopeVal qn = do
  r <- lookupValue qn
  let
    f l =
      case r of
        Right (LocalVName loc) -> LocalValue l loc
        Right (GlobalVName info) -> GlobalValue l info
        Left e -> ScopeError l e

  return $ fmap f qn

scopeVar :: Name SrcSpan -> ScopeM i (Name (Scoped SrcSpan))
scopeVar n = do
  sqn <- scopeVal (UnQual (ann n) n)
  case sqn of
    UnQual _ sn -> return sn
    _ -> error "scopeVar"

instance ScopeCheckR Module where
    scopeR (Module l mh os is ds) =
        Module (none l) <$>
            mapM scopeR mh <*>
            mapM scopeR os <*>
            mapM scopeR is <*>
            mapM scopeR ds
    scopeR m = unimplemented $ "scope: " ++ take 30 (prettyPrint m)

instance ScopeCheckR ModuleHead where
    scopeR (ModuleHead l n mw me) =
        ModuleHead (none l) (noScope n) (fmap noScope mw) <$> mapM scopeR me

instance ScopeCheckR ExportSpecList where
    scopeR (ExportSpecList l es) = ExportSpecList (none l) <$> mapM scopeR es

instance ScopeCheckR ExportSpec where
    scopeR (EVar l qn) = EVar (none l) <$> scopeVal qn
    scopeR (EAbs l qn) = EAbs (none l) <$> scopeTyCls qn
    scopeR (EThingAll l qn) = EThingAll (none l) <$> scopeTyCls qn
    scopeR (EThingWith l qn cns) = -- FIXME check that cns are exportable
      EThingWith (none l) <$> scopeTyCls qn <*> pure (map (fmap none) cns)
    scopeR e@EModuleContents{} = return $ noScope e

instance ScopeCheckR ModulePragma where
    scopeR = return . noScope

instance ScopeCheckR ImportDecl where
    scopeR = return . noScope  -- No interesting things to scope check here, but could do it if it's ever needed.

instance ScopeCheckM DeclHead where
    scopeM (DHead l n tvs) =
        DHead (none l) (fmap binder n) <$> mapM scopeM tvs
    scopeM (DHInfix l tv1 n tv2) =
        DHInfix (none l) <$> scopeM tv1 <*> pure (fmap binder n) <*> scopeM tv2
    scopeM (DHParen l dh) = DHParen (none l) <$> scopeM dh

addTyVarBind :: TyVarBind SrcSpan -> ScopeM Modify ()
addTyVarBind _tvs = return () -- XXX

instance ScopeCheckM TyVarBind where
    scopeM b@(KindedVar l n k) =
        KindedVar (none l) (fmap binder n) <$> scopeR k <* addTyVarBind b
    scopeM b@(UnkindedVar l n) =
        UnkindedVar (none l) (fmap binder n) <$ addTyVarBind b

instance ScopeCheckR Kind where
    scopeR = return . noScope

instance ScopeCheckR Decl where
    scopeR (TypeDecl l dh t) =
      delimit $ TypeDecl (none l) <$> scopeM dh <*> scopeR t
    -- TypeFamDecl
    scopeR (DataDecl l dn mc dh cs md) = delimit $ do
      dh' <- scopeM dh
      mc' <- mapM scopeR mc
      DataDecl (none l) (noScope dn) mc' dh' <$> mapM scopeR cs <*> mapM scopeR md

    -- GDataDecl
    -- DataFamDecl
    -- TypeInsDecl
    -- DataInsDecl
    -- GDataInsDecl
    scopeR (ClassDecl _l _mc _dh _fds _mds) = unimplemented "ClassDecl"
    scopeR (DerivDecl l mc ih) = DerivDecl (none l) <$> mapM scopeR mc <*> scopeR ih  -- XXX implicit tyvars
    scopeR (InfixDecl l a i ops) = return $ InfixDecl (none l) (noScope a) i (fmap noScope ops)  -- XXX check infix scope here?
    scopeR (DefaultDecl l ts) = DefaultDecl (none l) <$> mapM scopeR ts
    -- SpliceDecl
    scopeR (TypeSig l ns t) = delimit $
        TypeSig (none l) <$> mapM scopeVar ns <*> scopeR t  -- XXX this checks for stray sigs
    scopeR (FunBind l ms) = FunBind (none l) <$> mapM scopeR ms
    scopeR (PatBind l pat mbT rhs mbBinds) = delimit $ do
        pat' <- scopePat pat
        mbT' <- mapM scopeR mbT
        mbBinds' <- mapM scopeM mbBinds
        PatBind (none l) pat' mbT' <$> scopeR rhs <*> pure mbBinds'
    scopeR (ForImp l cc ms mn n t) = ForImp (none l) (noScope cc) (fmap noScope ms) mn (fmap binder n) <$> scopeR t
    scopeR (ForExp l cc    mn n t) = ForExp (none l) (noScope cc) mn <$> scopeVar n <*> scopeR t
    scopeR d = unimplemented $ "scope: " ++ take 30 (prettyPrint d)

instance ScopeCheckR QualConDecl where
    scopeR (QualConDecl l mtvs mc cd) = delimit $
        QualConDecl (none l) <$> (mapM (mapM scopeM) mtvs) <*> (mapM scopeR mc) <*> (scopeR cd)

instance ScopeCheckR ConDecl where
    scopeR (ConDecl l n ts) = ConDecl (none l) (fmap binder n) <$> mapM scopeR ts
    scopeR (InfixConDecl l t1 n t2) =
        InfixConDecl (none l) <$> scopeR t1 <*> pure (fmap binder n) <*> scopeR t2
    scopeR (RecDecl l n fs) = RecDecl (none l) (fmap binder n) <$> mapM scopeR fs

instance ScopeCheckR BangType where
    scopeR (BangedTy l t) = BangedTy (none l) <$> scopeR t
    scopeR (UnBangedTy l t) = UnBangedTy (none l) <$> scopeR t
    scopeR (UnpackedTy l t) = UnpackedTy (none l) <$> scopeR t

instance ScopeCheckR FieldDecl where
    scopeR (FieldDecl l ns t) =
        FieldDecl (none l) (fmap (fmap binder) ns) <$> scopeR t

instance ScopeCheckR Deriving where
    scopeR (Deriving l ih) = Deriving (none l) <$> mapM scopeR ih

instance ScopeCheckR Context where
    scopeR (CxSingle l a) = CxSingle (none l) <$> scopeR a
    scopeR (CxTuple l as) = CxTuple (none l) <$> mapM scopeR as
    scopeR (CxParen l c) = CxParen (none l) <$> scopeR c
    scopeR (CxEmpty l) = return $ CxEmpty (none l)

instance ScopeCheckR Asst where
    scopeR (ClassA l n ts) = ClassA (none l) <$> scopeCls n <*> mapM scopeR ts
    scopeR (InfixA l t1 n t2) = InfixA (none l) <$> scopeR t1 <*> scopeCls n <*> scopeR t2
    scopeR (IParam l n t) = IParam (none l) (fmap binder n) <$> scopeR t -- XXX not handled really
    scopeR (EqualP l t1 t2) = EqualP (none l) <$> scopeR t1 <*> scopeR t2

-- To support ScopedTypeVariables, we need to change this to ScopeCheckM.
-- There are other complexities as well...
instance ScopeCheckR Type where
    scopeR (TyForall l mtvs mc t) =
        delimit $ TyForall (none l) <$> mapM (mapM scopeM) mtvs <*> mapM scopeR mc <*> scopeR t
    scopeR (TyFun l t1 t2) = TyFun (none l) <$> scopeR t1 <*> scopeR t2
    scopeR (TyTuple l b ts) = TyTuple (none l) b <$> mapM scopeR ts
    scopeR (TyList l t) = TyList (none l) <$> scopeR t
    scopeR (TyApp l t1 t2) = TyApp (none l) <$> scopeR t1 <*> scopeR t2
    scopeR (TyVar l n) = TyVar (none l) <$> scopeTyVar n
    scopeR (TyCon l n) = TyCon (none l) <$> scopeTy n
    scopeR (TyParen l t) = TyParen (none l) <$> scopeR t
    scopeR (TyInfix l t1 n t2) =
        TyInfix (none l) <$> scopeR t1 <*> scopeTy n <*> scopeR t2
    scopeR (TyKind l t k) = TyKind (none l) <$> scopeR t <*> scopeR k

instance ScopeCheckR InstHead where
    scopeR (IHead l n ts) = IHead (none l) <$> scopeCls n <*> mapM scopeR ts
    scopeR (IHInfix l t1 n t2) = IHInfix (none l) <$> scopeR t1 <*> scopeCls n <*> scopeR t2
    scopeR (IHParen l i) = IHParen (none l) <$> scopeR i

instance ScopeCheckR Match where
    scopeR (Match l n ps rhs mb) = delimit $ do
        ps' <- scopePats ps
        mb' <- mapM scopeM mb
        rhs' <- scopeR rhs
        return $ Match (none l) (fmap binder n) ps' rhs' mb'
    scopeR (InfixMatch l p n ps rhs mb) = do
        Match l' n' (p':ps') rhs' mb' <- scopeR $ Match l n (p:ps) rhs mb
        return $ InfixMatch l' p' n' ps' rhs' mb'

-- Often there are many patterns which should bring variables in scope
-- simultaneously (atomically).
--
-- Thus, we make scopePat a separate function (rather than a method of
-- ScopeCheckM instance for Pat) to make scope-checking of patterns explicit.
--
-- It's then possible to grep for scopePat and make sure that we don't use it with
-- mapM anywhere.
scopePat  ::  Pat SrcSpan ->  ScopeM Modify (Pat (Scoped SrcSpan))
scopePats :: [Pat SrcSpan] -> ScopeM Modify [Pat (Scoped SrcSpan)]
(scopePat, scopePats) =
    (\p ->       scopePatR p  <* addVars (getBound p),
     \ps -> mapM scopePatR ps <* addVars (getBound ps))
    where
    scopePatR :: Pat SrcSpan -> ScopeM i (Pat (Scoped SrcSpan))
    scopePatR (PVar l n) = return $ PVar (none l) (fmap binder n)
    scopePatR (PLit l k) = PLit (none l) <$> scopeR k
    scopePatR (PNeg l p) = PNeg (none l) <$> scopePatR p
    scopePatR (PNPlusK l n i) = return $ PNPlusK (none l) (fmap binder n) i
    scopePatR (PInfixApp l p1 n p2) = PInfixApp (none l) <$> scopePatR p1 <*> scopeVal n <*> scopePatR p2
    scopePatR (PApp l n ps) = PApp (none l) <$> scopeVal n <*> mapM scopePatR ps
    scopePatR (PTuple l ps) = PTuple (none l) <$> mapM scopePatR ps
    scopePatR (PList l ps) = PList (none l) <$> mapM scopePatR ps
    scopePatR (PParen l p) = PParen (none l) <$> scopePatR p
    -- PRec
    scopePatR (PAsPat l n p) = PAsPat (none l) (fmap binder n) <$> scopePatR p
    scopePatR (PWildCard l) = return $ PWildCard (none l)
    scopePatR (PIrrPat l p) = PIrrPat (none l) <$> scopePatR p
    scopePatR (PatTypeSig l p t) = PatTypeSig (none l) <$> scopePatR p <*> scopeR t
    scopePatR (PViewPat l expr pat) = PViewPat (none l) <$> scopeR expr <*> scopePatR pat
    -- PRPat
    -- PXTag
    -- PXETag
    -- PXPcdata
    -- PXPatTag
    -- PXRPats
    -- PExplTypeArg
    -- PQuasiQuote
    scopePatR (PBangPat l p) = PBangPat (none l) <$> scopePatR p
    scopePatR p = unimplemented $ "scope: " ++ take 30 (prettyPrint p)

instance ScopeCheckR Literal where
    scopeR = return . noScope

instance ScopeCheckR Rhs where
    scopeR (UnGuardedRhs l e) = UnGuardedRhs (none l) <$> scopeR e
    scopeR (GuardedRhss l gs) = GuardedRhss (none l) <$> mapM scopeR gs

instance ScopeCheckR GuardedRhs where
    scopeR (GuardedRhs l ss e) = delimit $
        GuardedRhs (none l) <$> mapM scopeM ss <*> scopeR e

instance ScopeCheckM Stmt where
    scopeM (Generator l pat expr) = do
      -- NB: expr can't reference vars bound by pat
      expr' <- scopeR expr
      pat' <- scopePat pat
      return $ Generator (none l) pat' expr'
    scopeM (Qualifier l expr) = Qualifier (none l) <$> scopeR expr
    scopeM (LetStmt l bnds) = LetStmt (none l) <$> scopeM bnds
    scopeM RecStmt {} = unimplemented "scope: RecStmt"

instance ScopeCheckR Exp where
    scopeR (Var l n) = Var (none l) <$> scopeVal n
    -- IPVar
    scopeR (Con l n) = Con (none l) <$> scopeVal n
    scopeR (Lit l i) = Lit (none l) <$> scopeR i
    scopeR (InfixApp l e1 o e2) = InfixApp (none l) <$> scopeR e1 <*> scopeR o <*> scopeR e2
    scopeR (App l e1 e2) = App (none l) <$> scopeR e1 <*> scopeR e2
    scopeR (NegApp l e) = NegApp (none l) <$> scopeR e
    scopeR (Lambda l ps e) = delimit $ Lambda (none l) <$> scopePats ps <*> scopeR e
    scopeR (Let l b e) = delimit $ Let (none l) <$> scopeM b <*> scopeR e
    scopeR (If l e1 e2 e3) = If (none l) <$> scopeR e1 <*> scopeR e2 <*> scopeR e3
    scopeR (Case l e as) = Case (none l) <$> scopeR e <*> mapM scopeR as
    scopeR (Do l ss) = delimit $ Do (none l) <$> mapM scopeM ss
    scopeR (MDo l ss) = delimit $ MDo (none l) <$> (introduceAllBindings *> mapM scopeM ss)
        where
        introduceAllBindings = sequence [ addVars $ getBound pat | Generator _ pat _ <- ss ]
    scopeR (Tuple l es) = Tuple (none l) <$> mapM scopeR es
    scopeR (TupleSection l es) = TupleSection (none l) <$> mapM (mapM scopeR) es
    scopeR (List l es) = List (none l) <$> mapM scopeR es
    scopeR (Paren l e) = Paren (none l) <$> scopeR e
    scopeR (LeftSection l e o) = LeftSection (none l) <$> scopeR e <*> scopeR o
    scopeR (RightSection l o e) = RightSection (none l) <$> scopeR o <*> scopeR e
    scopeR (RecConstr l n fs) = RecConstr (none l) <$> scopeVal n <*> mapM scopeR fs
    scopeR (RecUpdate l e fs) = RecUpdate (none l) <$> scopeR e <*> mapM scopeR fs
    scopeR (EnumFrom l e) = EnumFrom (none l) <$> scopeR e
    scopeR (EnumFromTo l e1 e2) = EnumFromTo (none l) <$> scopeR e1 <*> scopeR e2
    scopeR (EnumFromThen l e1 e2) = EnumFromThen (none l) <$> scopeR e1 <*> scopeR e2
    scopeR (EnumFromThenTo l e1 e2 e3) = EnumFromThenTo (none l) <$> scopeR e1 <*> scopeR e2 <*> scopeR e3
    scopeR (ListComp l e ss) = delimit $ do
      ss' <- scopeQualStmts ss
      ListComp (none l) <$> scopeR e <*> pure ss'
    -- ParComp
    scopeR (ExpTypeSig l e t) = ExpTypeSig (none l) <$> scopeR e <*> scopeR t
    -- VarQuote
    -- TypQuote
    -- BracketExp
    -- SpliceExp
    -- QuasiQuote
    -- XTag
    -- XETag
    -- XPcdata
    -- XExpTag
    -- ... XXX pragmas, arrows
    scopeR e = unimplemented $ "scope: " ++ take 30 (prettyPrint e)

scopeQualStmts :: [QualStmt SrcSpan] -> ScopeM Modify [QualStmt (Scoped SrcSpan)]
scopeQualStmts = unimplemented "scopeQualStmts"

instance ScopeCheckR QOp where
    scopeR (QVarOp l n) = QVarOp (none l) <$> scopeVal n
    scopeR (QConOp l n) = QConOp (none l) <$> scopeVal n

instance ScopeCheckR Alt where
    scopeR (Alt l pat alts mbBinds) = delimit $ do
        pat' <- scopePat pat
        mbBinds' <- mapM scopeM mbBinds
        alts' <- scopeR alts
        return $ Alt (none l) pat' alts' mbBinds'

instance ScopeCheckR GuardedAlts where
    scopeR (UnGuardedAlt l expr) = UnGuardedAlt (none l) <$> scopeR expr
    scopeR (GuardedAlts l alts) = GuardedAlts (none l) <$> mapM scopeR alts

instance ScopeCheckR GuardedAlt where
    scopeR (GuardedAlt l stmts expr) = delimit $
        GuardedAlt (none l) <$> mapM scopeM stmts <*> scopeR expr

instance ScopeCheckR FieldUpdate where
    scopeR (FieldUpdate l field expr) =
        FieldUpdate (none l) <$> scopeVal field <*> scopeR expr
    scopeR _ = unimplemented "scope: FieldUpdate"

instance ScopeCheckM Binds where
    scopeM (BDecls l ds) = do
      addVars $ getBound ds
      BDecls (none l) <$> mapM scopeR ds
    scopeM IPBinds {} = unimplemented "scope: IPBinds"

unimplemented :: String -> a
unimplemented s = error $ "Unimplemented: " ++ s
