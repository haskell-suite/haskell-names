{-# LANGUAGE PatternGuards, DeriveDataTypeable #-}
module Language.Haskell.Modules.Scope(
    scopeAnalysis,
    Scoped(..), getOriginalName,
    scopeModule,
    runS,
    getScopeErrors) where

-- TODO
--  check for multiple bindings in forall and patterns
--  with scopedtyvar use the forall to bind tyvars in the function body
--  need better getBound for wildcard patterns
--  resolve operators after scope check

import Control.Applicative
import Control.Arrow((***))
import Control.Monad
import Control.Monad.State
import Data.Data
import Data.Either
import Data.Function
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Data.Traversable (traverse)
import qualified Data.Set as S

--import qualified Language.Haskell.Exts.Fixity as F
import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Exts.Annotated hiding (fixities)

import Language.Haskell.Modules.Error
import Language.Haskell.Modules.Flags
import Language.Haskell.Modules.MonadModule
import Language.Haskell.Modules.Recursive
import Language.Haskell.Modules.ResolveMonad(ModuleSet)
import Language.Haskell.Modules.ScopeMonad
import Language.Haskell.Modules.SymbolTable
import Language.Haskell.Modules.SyntaxUtils

data Scoped l
    = Global     { sLoc :: l, sOrginalName :: QName SrcLoc }
    | Local      { sLoc :: l, sDefLoc :: SrcLoc }
    | Binder     { sLoc :: l }
    | None       { sLoc :: l }
    | ScopeError { sLoc :: l, serr :: Msg }
    deriving (Show, Typeable, Data)

instance (SrcInfo l) => SrcInfo (Scoped l) where
    toSrcInfo _l1 _ss _l2 = unimplemented "toSrcInfo Scoped"
    fromSrcInfo _si = unimplemented "fromSrcInfo Scoped"
    getPointLoc = getPointLoc . sLoc
    fileName = fileName . sLoc
    startLine = startLine . sLoc
    startColumn = startColumn . sLoc

getOriginalName :: (Annotated a) => a (Scoped l) -> Maybe (QName SrcLoc)
getOriginalName x =
    case ann x of
    Global { sOrginalName = n } -> Just n
    _ -> Nothing

scopeAnalysis :: Flags -> ModuleSet -> ([Msg], [Module (Scoped SrcSpan)])
scopeAnalysis flags = (sortBy (compare `on` msgLoc) *** concat) . runS flags . mapM scopeGroup . groupModules True

scopeGroup :: ModuleSet -> S [Module (Scoped SrcSpan)]
scopeGroup [Left s] = fmap return $ scopeSummary s
scopeGroup [Right m] = fmap return $ scopeModule m
scopeGroup _ = unimplemented "mutually recursive modules"

scopeSummary :: ModuleSummary -> S (Module (Scoped SrcSpan))
scopeSummary = unimplemented "scopeSummary"

scopeModule :: Module SrcSpan -> S (Module (Scoped SrcSpan))
scopeModule m = do
    let mname = dropAnn $ getModuleName m
        decls = getModuleDecls m
        fixityTable = getFixities decls
        syms@(vs, ts) = partitionEithers $ concatMap (getTopDeclNames mname fixityTable) decls
        tns = map (qNameToName . st_origName) ts                         -- type&class names
        vns = map (qNameToName . sv_origName) vs
        vns' = [ qNameToName n | SymValue { sv_origName = n } <- vs ]
        sigs = [ n | TypeSig _ ns _ <- decls, n <- ns ]
        fixes = [ opName o | InfixDecl _ _ _ ops <- decls, o <- ops ]
        ans = tns ++ vns
    dupCheck "type/class definition" tns                 -- Check for duplicate types/classes
    dupCheck "value definition" vns                      -- Check for duplicate values
    strayCheck "type signature" vns' sigs                -- Check for stray type signatures
    dupCheck "type signature" sigs                       -- Check for duplicate type signatures
    strayCheck "infix declaration" ans fixes             -- Check for stray infix declarations
    dupCheck "infix declaration" fixes                   -- Check for duplicate infix declarations

-- XXX return symbols instead?
    processImports (ann m) (getImports m)
    addUnqualifiedSymbols syms
    addQualifiedSymbols mname syms

    st <- getSymbolTable
    let m' = scope st m

    -- Remember module for future processing.
    addModuleSymbols mname =<< filterExports (getExportSpecList m') syms

    --- XXX Clear symbol table

    return m'

-----------------------------------------------------------------------------

processImports :: (SrcInfo l) => l -> [ImportDecl l] -> S ()
processImports l is = do
    flgs <- getFlags
    let is' = if any ((=~= pm) . importModule) is || not (f_usePrelude flgs) then is else ip : is
        ip = ImportDecl l pm False False Nothing Nothing Nothing
        pm = prelude_mod l
    mapM_ processImport is'

-- Import all identifiers and add them to the global symbol table.
processImport :: (SrcInfo l) => ImportDecl l -> S ()
processImport i = do
    let im = importModule i
    mbSyms <- getModuleSymbols $ dropAnn im
    case mbSyms of
      Nothing -> return () -- FIXME: issue a warning
      Just (vs, ts) -> do
        let mdl = fromMaybe im $ importAs i
            spec = importSpecs i
        vs' <- filterValues spec vs
        ts' <- filterTypes spec ts
        let syms = (vs', ts')
        when (not $ importQualified i) $
            addUnqualifiedSymbols syms
        addQualifiedSymbols mdl syms

filterValues :: (SrcInfo l) => Maybe (ImportSpecList l) -> SymValueInfos -> S SymValueInfos
filterValues Nothing vs = return vs
filterValues (Just (ImportSpecList _ hide ss)) vs = do
    let ns = [ n | IVar _ n <- ss ] ++
             [ unCName cn | IThingWith _ _ cns <- ss, cn <- cns ]
        impValues = S.fromList $ map dropAnn ns
        impTypes = S.fromList [ dropAnn n | IThingAll _ n <- ss ]
        vns = S.fromList $ map (dropAnn . qNameToName . sv_origName) vs
        chk n | not (S.member (dropAnn n) vns) = scopeMsg $ msgError (getPointLoc $ ann n) "Module does not export value" [msgArg n]
        chk _ = return ()
        found = (`S.member` impValues) . dropAnn . qNameToName . sv_origName
        foundT = (== Just True) . fmap ((`S.member` impTypes) . qNameToName) . sv_parent
    mapM_ chk ns
    return $ filter (\ i -> (found i || foundT i) /= hide) vs

filterTypes :: (SrcInfo l) => Maybe (ImportSpecList l) -> SymTypeInfos -> S SymTypeInfos
filterTypes Nothing ts = return ts
filterTypes (Just (ImportSpecList _ hide ss)) ts = do
    let ns = [ n | IThingAll _ n <- ss ] ++
             [ n | IThingWith _ n _ <- ss ] ++
             [ n | IAbs _ n <- ss ]
        impTypes = S.fromList $ map dropAnn ns
        found = (`S.member` impTypes) . dropAnn . qNameToName . st_origName
        tns = S.fromList $ map (dropAnn . qNameToName . st_origName) ts
        chk n | not (S.member (dropAnn n) tns) = scopeMsg $ msgError (getPointLoc $ ann n) "Module does not export type/class" [msgArg n]
        chk _ = return ()
    mapM_ chk ns
    return $ filter ((/= hide) . found) ts

-----------------------------------------------------------------------------

type FixityTable = [(Name (), SymFixity)]

getFixities :: [Decl SrcSpan] -> FixityTable
getFixities decls = [ (dropAnn $ opName o, (getAssoc a, fromMaybe defaultPrecedence mp)) | InfixDecl _ a mp ops <- decls, o <- ops ]
  where getAssoc (AssocNone  _) = S.AssocNone
        getAssoc (AssocLeft  _) = S.AssocLeft
        getAssoc (AssocRight _) = S.AssocRight

defaultPrecedence :: Int
defaultPrecedence = 9

dupCheck :: (SrcInfo l) => String -> [Name l] -> S ()
dupCheck msg = mapM_ report . filter ((> 1) . length) . groupBy ((==) `on` dropAnn) . sortBy (compare `on` dropAnn) 
  where report (n:ns) = scopeMsg $ msgError (getPointLoc $ ann n) ("Duplicate " ++ msg ++ "s") $ map msgArgLoc ns
        report [] = internalError "dupCheck"

strayCheck :: (SrcInfo l') => String -> [Name l] -> [Name l'] -> S ()
strayCheck msg as = mapM_ check1
  where s = S.fromList $ map dropAnn as
        check1 n = unless (dropAnn n `S.member` s) $ scopeMsg $ msgError (getPointLoc $ ann n) ("Stray " ++ msg) [msgArg n]

filterExports :: Maybe (ExportSpecList (Scoped l)) -> Symbols-> S Symbols
filterExports Nothing l = return l
filterExports (Just (ExportSpecList _ specs)) (vs, ts) = return (filter expValue vs, filter expType ts)
  where expValue (SymConstructor { sv_typeName  = qn }) | qn `S.member` allTys = True
        expValue (SymSelector    { sv_typeName  = qn }) | qn `S.member` allTys = True
        expValue (SymMethod      { sv_className = qn }) | qn `S.member` allTys = True
        expValue i = dropAnn (sv_origName i) `S.member` vars
        vars = fromList $ [ orig n | EVar _ n <- specs ] ++
                          [ Just $ copyQual qn $ dropAnn $ unCName cn | EThingWith _ n cns <- specs, cn <- cns, Just qn <- [orig n] ]
        expType  i = dropAnn (st_origName i) `S.member` tys
        tys = fromList ( [ orig n | EAbs _ n <- specs ] ++
                         [ orig n | EThingWith _ n _ <- specs ])
                `S.union` allTys
        allTys = fromList [ orig n | EThingAll _ n <- specs ]
        fromList = S.fromList . catMaybes
        orig :: (Annotated a) => a (Scoped l) -> Maybe (QName ())
        orig = fmap dropAnn . getOriginalName
        copyQual (Qual _ m _) n = Qual (ann n) m n
        copyQual _ _ = internalError "copyQual"
        -- XXX EModuleContents

-- Extract names that get bound by a top level declaration.
getTopDeclNames :: ModuleName l -> FixityTable -> Decl SrcSpan -> [Either SymValueInfo SymTypeInfo]
getTopDeclNames mdl ftbl d =
    case d of
    TypeDecl _ dh _ ->
        let tn = hname dh
        in  [ Right (SymType        { st_origName = qname tn, st_fixity = fixity tn })]
    TypeFamDecl _ dh _ ->
        let tn = hname dh
        in  [ Right (SymTypeFam     { st_origName = qname tn, st_fixity = fixity tn })]
    DataDecl _ _ _ dh _ _ ->
        let dn = hname dh
            dq = qname dn
            (cs, fs) = partition isCon $ getBound d
            as = cs ++ nub fs  -- Ignore multiple selectors for now
        in    Right (SymData        { st_origName = dq,       st_fixity = fixity dn }) :
            [ if isCon cn then
              Left  (SymConstructor { sv_origName = qname cn, sv_fixity = fixity cn, sv_typeName = dropAnn dq }) else
              Left  (SymSelector    { sv_origName = qname cn, sv_fixity = fixity cn, sv_typeName = dropAnn dq })
            | cn <- as ]
    GDataDecl _ _ _ dh _ _ _ ->
        let dn = hname dh
            cq = qname dn
            (cs, fs) = partition isCon $ getBound d
            as = cs ++ nub fs  -- Ignore multiple selectors for now
        in    Right (SymData        { st_origName = cq,       st_fixity = fixity dn }) :
            [ if isCon cn then
              Left  (SymConstructor { sv_origName = qname cn, sv_fixity = fixity cn, sv_typeName = dropAnn cq }) else
              Left  (SymSelector    { sv_origName = qname cn, sv_fixity = fixity cn, sv_typeName = dropAnn cq })
            | cn <- as ]
    ClassDecl _ _ _ _ mds ->
        let ms = getBound d
            cn = getDeclHeadName d
            cq = qname cn
            cdecls = fromMaybe [] mds
        in    Right (SymClass       { st_origName = cq,       st_fixity = fixity cn }) :
            [ Right (SymTypeFam     { st_origName = qname dn, st_fixity = fixity dn }) | ClsTyFam   _   dh _ <- cdecls, let dn = hname dh ] ++
            [ Right (SymDataFam     { st_origName = qname tn, st_fixity = fixity tn }) | ClsDataFam _ _ dh _ <- cdecls, let tn = hname dh ] ++
            [ Left  (SymMethod      { sv_origName = qname mn, sv_fixity = fixity mn, sv_className = dropAnn cq }) | mn <- ms ]
--    TypeSig _ ns _ ->
--            [ Left  (SymValue       { sv_origName = qname vn, sv_fixity = fixity vn } | vn <- ns ]
    FunBind _ ms ->
        let vn : _ = getBound ms
        in  [ Left  (SymValue       { sv_origName = qname vn, sv_fixity = fixity vn }) ]
    PatBind _ p _ _ _ ->
            [ Left  (SymValue       { sv_origName = qname vn, sv_fixity = fixity vn }) | vn <- getBound p ]
    ForImp _ _ _ _ fn _ ->
            [ Left  (SymForeign     { sv_origName = qname fn, sv_fixity = fixity fn }) ]
    _ ->    []
  where ModuleName _ smdl = mdl
        qname n = setAnn (getPointLoc $ ann n) $ Qual undefined (ModuleName undefined smdl) n
        fixity n = lookup (dropAnn n) ftbl
        hname = fst . splitDeclHead

-----------------------------------------------------------------------------

getScopeErrors :: [Module (Scoped SrcSpan)] -> [Msg]
getScopeErrors ms = [ m | ScopeError _ m <- universeBi ms :: [Scoped SrcSpan] ]

-----------------------------------------------------------------------------

class {-(Annotated a) => -} ScopeCheck a where
    scope :: SymbolTable -> a SrcSpan -> a (Scoped SrcSpan)

scopeM :: ScopeCheck a => a SrcSpan -> State SymbolTable (a (Scoped SrcSpan))
scopeM x = scope <$> get <*> pure x

none :: l -> Scoped l
none = None

binder :: l -> Scoped l
binder = Binder

noScope :: (Annotated a) => a l -> a (Scoped l)
noScope = fmap None

scopeX :: (SymTypeInfo -> Maybe String) -> String -> SymbolTable -> QName SrcSpan -> QName (Scoped SrcSpan)
scopeX ok w st qn =
    let f l = case symTypeLookup qn st of
              Nothing -> ScopeError l $ msgError (ann qn) ("Undefined " ++ w ++ " identifier") [msgArg qn]
              Just i -> case ok i of
                        Nothing -> Global l (st_origName i)
                        Just s -> ScopeError l $ msgError (ann qn) s [msgArg qn]
    in  fmap f qn

scopeTyCls :: SymbolTable -> QName SrcSpan -> QName (Scoped SrcSpan)
scopeTyCls = scopeX (const Nothing) "type/class"

scopeCls :: SymbolTable -> QName SrcSpan -> QName (Scoped SrcSpan)
scopeCls = scopeX cls "class"
  where cls SymClass{} = Nothing
        cls _ = Just "Type used as a class"

scopeTy :: SymbolTable -> QName SrcSpan -> QName (Scoped SrcSpan)
scopeTy = scopeX cls "type"
  where cls SymClass{} = Just "Class used as a type"
        cls _ = Nothing

scopeTyVar :: SymbolTable -> Name SrcSpan -> Name (Scoped SrcSpan)
scopeTyVar st n = fixup $ scopeX cls "type variable" st (UnQual (ann n) n)
  where cls SymClass{} = Just "Class used as a type"
        cls _ = Nothing
        fixup = fmap lcl . qNameToName
        lcl (Global l qn) = Local l (ann qn)
        lcl l = l

-- XXX Handle local symbols
scopeVal :: SymbolTable -> QName SrcSpan -> QName (Scoped SrcSpan)
scopeVal st qn =
    let f l = case symValueLookup qn st of
              Nothing -> ScopeError l $ msgError (ann qn) "Undefined value identifier" [msgArg qn]
              Just i  -> Global l (sv_origName i)
    in  fmap f qn

--- XXX
scopeVar :: SymbolTable -> Name SrcSpan -> Name (Scoped SrcSpan)
scopeVar st n =
    let f l = case symValueLookup (UnQual (ann n) n) st of
              Nothing -> ScopeError l $ msgError (ann n) "Undefined value identifier" [msgArg n]
              Just i  -> Local l (ann $ sv_origName i)
    in  fmap f n

instance ScopeCheck Module where
    scope st (Module l mh os is ds) = Module (none l) (fmap (scope st) mh) (fmap (scope st) os) (fmap (scope st) is) (fmap (scope st) ds)
    scope _ m = unimplemented $ "scope: " ++ take 30 (prettyPrint m)

instance ScopeCheck ModuleHead where
    scope st (ModuleHead l n mw me) = ModuleHead (none l) (noScope n) (fmap noScope mw) (fmap (scope st) me)

instance ScopeCheck ExportSpecList where
    scope st (ExportSpecList l es) = ExportSpecList (none l) (fmap (scope st) es)

instance ScopeCheck ExportSpec where
    scope st (EVar l qn) = EVar (none l) (scopeVal st qn)
    scope st (EAbs l qn) = EAbs (none l) (scopeTyCls st qn)
    scope st (EThingAll l qn) = EThingAll (none l) (scopeTyCls st qn)
    scope _st (EThingWith _l _qn _cns) = unimplemented "EThingWith"  -- check that cns are exportable
    scope _ e@EModuleContents{} = noScope e

instance ScopeCheck ModulePragma where
    scope _ = noScope

instance ScopeCheck ImportDecl where
    scope _ = noScope  -- No interesting things to scope check here, but could do it if it's ever needed.

scopeDeclHead :: DeclHead SrcSpan -> State SymbolTable (DeclHead (Scoped SrcSpan))
scopeDeclHead (DHead l n tvs) =
  state $ \st -> (DHead (none l) (fmap binder n) (fmap (scope st) tvs), addTyVarBinds tvs st)
scopeDeclHead (DHInfix l tv1 n tv2) =
  state $ \st -> (DHInfix (none l) (scope st tv1) (fmap binder n) (scope st tv2), addTyVarBinds [tv1, tv2] st)
scopeDeclHead (DHParen l dh) = DHParen (none l) <$> scopeDeclHead dh

addTyVarBinds :: [TyVarBind SrcSpan] -> SymbolTable -> SymbolTable
addTyVarBinds _tvs st = st -- XXX

addVars :: [Name SrcSpan] -> SymbolTable -> SymbolTable
addVars _vs st = st -- XXX

instance ScopeCheck TyVarBind where
    scope st (KindedVar l n k) = KindedVar (none l) (fmap binder n) (scope st k)
    scope  _ (UnkindedVar l n) = UnkindedVar (none l) (fmap binder n)

instance ScopeCheck Kind where
    scope _ k = noScope k

instance ScopeCheck Decl where
    scope st (TypeDecl l dh t) =
      flip evalState st $ TypeDecl (none l) <$> scopeDeclHead dh <*> scopeM t
    -- TypeFamDecl
    scope st (DataDecl l dn mc dh cs md) = flip evalState st $ do
      dh' <- scopeDeclHead dh
      mc' <- scopeMaybeContext mc
      DataDecl (none l) (noScope dn) mc' dh' <$> traverse scopeM cs <*> traverse scopeM md

    -- GDataDecl
    -- DataFamDecl
    -- TypeInsDecl
    -- DataInsDecl
    -- GDataInsDecl
    scope _st (ClassDecl _l _mc _dh _fds _mds) = unimplemented "ClassDecl"
    scope st (DerivDecl l mc ih) = DerivDecl (none l) (fmap (scope st) mc) (scope st ih)  -- XXX implicit tyvars
    scope  _ (InfixDecl l a i ops) = InfixDecl (none l) (noScope a) i (fmap noScope ops)  -- XXX check infix scope here?
    scope st (DefaultDecl l ts) = DefaultDecl (none l) (fmap (scopeType st) ts)
    -- SpliceDecl
    scope st (TypeSig l ns t) = TypeSig (none l) (fmap (scopeVar st) ns) (scopeType st t)  -- XXX this checks for stray sigs
    scope st (FunBind l ms) = FunBind (none l) (fmap (scope st) ms)
    scope st (PatBind l pat mbT rhs mbBinds) =
      PatBind
        (none l)
        (scope st pat)
        (fmap (scopeType st) mbT)
        (scope st rhs)
        (fmap (scope st) mbBinds)
    scope st (ForImp l cc ms mn n t) = ForImp (none l) (noScope cc) (fmap noScope ms) mn (fmap binder n) (scopeType st t)
    scope st (ForExp l cc    mn n t) = ForExp (none l) (noScope cc)                   mn (scopeVar st n) (scopeType st t)
    scope _ d = unimplemented $ "scope: " ++ take 30 (prettyPrint d)

scopeMaybeContext :: Maybe (Context SrcSpan) -> State SymbolTable (Maybe (Context (Scoped SrcSpan)))
scopeMaybeContext mc = state $ \st -> (fmap (scope st) mc, st)

instance ScopeCheck QualConDecl where
    scope st (QualConDecl l mtvs mc cd) = QualConDecl (none l) (fmap (fmap (scope st)) mtvs) (fmap (scope st') mc) (scope st' cd)
                                          where st' = maybe st (flip addTyVarBinds st) mtvs

instance ScopeCheck ConDecl where
    scope st (ConDecl l n ts) = ConDecl (none l) (fmap binder n) (fmap (scope st) ts)
    scope st (InfixConDecl l t1 n t2) = InfixConDecl (none l) (scope st t1) (fmap binder n) (scope st t2)
    scope st (RecDecl l n fs) = RecDecl (none l) (fmap binder n) (fmap (scope st) fs)

instance ScopeCheck BangType where
    scope st (BangedTy l t) = BangedTy (none l) (scope st t)
    scope st (UnBangedTy l t) = UnBangedTy (none l) (scope st t)
    scope st (UnpackedTy l t) = UnpackedTy (none l) (scope st t)

instance ScopeCheck FieldDecl where
    scope st (FieldDecl l ns t) = FieldDecl (none l) (fmap (fmap binder) ns) (scope st t)

instance ScopeCheck Deriving where
    scope st (Deriving l ih) = Deriving (none l) (fmap (scope st) ih)

instance ScopeCheck Context where
    scope st (CxSingle l a) = CxSingle (none l) (scope st a)
    scope st (CxTuple l as) = CxTuple (none l) (fmap (scope st) as)
    scope st (CxParen l c) = CxParen (none l) (scope st c)
    scope  _ (CxEmpty l) = CxEmpty (none l)

instance ScopeCheck Asst where
    scope st (ClassA l n ts) = ClassA (none l) (scopeCls st n) (fmap (scope st) ts)
    scope st (InfixA l t1 n t2) = InfixA (none l) (scope st t1) (scopeCls st n) (scope st t2)
    scope st (IParam l n t) = IParam (none l) (fmap binder n) (scope st t) -- XXX not handled really
    scope st (EqualP l t1 t2) = EqualP (none l) (scope st t1) (scope st t2)

instance ScopeCheck Type where
    scope st (TyForall l mtvs mc t) = TyForall (none l) (fmap (fmap (scope st)) mtvs) (fmap (scope st') mc) (scope st' t)
                                      where st' = maybe st (flip addTyVarBinds st) mtvs
    scope st (TyFun l t1 t2) = TyFun (none l) (scope st t1) (scope st t2)
    scope st (TyTuple l b ts) = TyTuple (none l) b (fmap (scope st) ts)
    scope st (TyList l t) = TyList (none l) (scope st t)
    scope st (TyApp l t1 t2) = TyApp (none l) (scope st t1) (scope st t2)
    scope st (TyVar l n) = TyVar (none l) (scopeTyVar st n)
    scope st (TyCon l n) = TyCon (none l) (scopeTy st n)
    scope st (TyParen l t) = TyParen (none l) (scope st t)
    scope st (TyInfix l t1 n t2) = TyInfix (none l) (scope st t1) (scopeTy st n) (scope st t2)
    scope st (TyKind l t k) = TyKind (none l) (scope st t) (scope st k)

-- XXX bind any free tyvars tyvars
scopeType :: SymbolTable -> Type SrcSpan -> Type (Scoped SrcSpan)
scopeType st t = scope st t

instance ScopeCheck InstHead where
    scope st (IHead l n ts) = IHead (none l) (scopeCls st n) (fmap (scope st) ts)
    scope st (IHInfix l t1 n t2) = IHInfix (none l) (scope st t1) (scopeCls st n) (scope st t2)
    scope st (IHParen l i) = IHParen (none l) (scope st i)

instance ScopeCheck Match where
    scope st (Match l n ps rhs mb) = flip evalState st $ do
      ps' <- scopePats ps
      mb' <- traverse scopeBinds mb
      rhs' <- scopeM rhs
      return $ Match (none l) (fmap binder n) ps' rhs' mb'
    scope st (InfixMatch l p n ps rhs mb) = InfixMatch l' p' n' ps' rhs' mb'
      where Match l' n' (p':ps') rhs' mb' = scope st $ Match l n (p:ps) rhs mb

scopePats :: [Pat SrcSpan] -> State SymbolTable [Pat (Scoped SrcSpan)]
scopePats ps = state $ \st -> (fmap (scope st) ps, addVars (getBound ps) st)

scopePat :: Pat SrcSpan -> State SymbolTable (Pat (Scoped SrcSpan))
scopePat p = state $ \st -> (scope st p, addVars (getBound p) st)

scopeBinds :: Binds SrcSpan -> State SymbolTable (Binds (Scoped SrcSpan))
scopeBinds (BDecls l ds) = do
  modify $ addVars $ getBound ds
  BDecls (none l) <$> mapM scopeM ds
scopeBinds IPBinds {} = unimplemented "scope: IPBinds"

instance ScopeCheck Pat where
    scope  _ (PVar l n) = PVar (none l) (fmap binder n)
    scope st (PLit l k) = PLit (none l) (scope st k)
    scope st (PNeg l p) = PNeg (none l) (scope st p)
    scope  _ (PNPlusK l n i) = PNPlusK (none l) (fmap binder n) i
    scope st (PInfixApp l p1 n p2) = PInfixApp (none l) (scope st p1) (scopeVal st n) (scope st p2)
    scope st (PApp l n ps) = PApp (none l) (scopeVal st n) (fmap (scope st) ps)
    scope st (PTuple l ps) = PTuple (none l) (fmap (scope st) ps)
    scope st (PList l ps) = PList (none l) (fmap (scope st) ps)
    scope st (PParen l p) = PParen (none l) (scope st p)
    -- PRec
    scope st (PAsPat l n p) = PAsPat (none l) (fmap binder n) (scope st p)
    scope  _ (PWildCard l) = PWildCard (none l)
    scope st (PIrrPat l p) = PIrrPat (none l) (scope st p)
    scope st (PatTypeSig l p t) = PatTypeSig (none l) (scope st p) (scopeType st t)
    -- PViewPat
    -- PRPat
    -- PXTag
    -- PXETag
    -- PXPcdata
    -- PXPatTag
    -- PXRPats
    -- PExplTypeArg
    -- PQuasiQuote
    scope st (PBangPat l p) = PBangPat (none l) (scope st p)
    scope _ p = unimplemented $ "scope: " ++ take 30 (prettyPrint p)

instance ScopeCheck Literal where
    scope _ = noScope

instance ScopeCheck Rhs where
    scope st (UnGuardedRhs l e) = UnGuardedRhs (none l) (scope st e)
    scope st (GuardedRhss l gs) = GuardedRhss (none l) (fmap (scope st) gs)

instance ScopeCheck GuardedRhs where
    scope st (GuardedRhs l ss e) = flip evalState st $ GuardedRhs (none l) <$> scopeStmts ss <*> scopeM e

scopeStmts :: [Stmt SrcSpan] -> State SymbolTable [Stmt (Scoped SrcSpan)]
scopeStmts = mapM scopeStmt

scopeStmt :: Stmt SrcSpan -> State SymbolTable (Stmt (Scoped SrcSpan))
scopeStmt (Generator l pat expr) = do
  pat' <- scopePat pat
  expr' <- scopeM expr
  return $ Generator (none l) pat' expr'
scopeStmt (Qualifier l expr) = Qualifier (none l) <$> scopeM expr
scopeStmt (LetStmt l bnds) = undefined
scopeStmt RecStmt {} = unimplemented "scope: RecStmt"

instance ScopeCheck Exp where
    scope st (Var l n) = Var (none l) (scopeVal st n)
    -- IPVar
    scope st (Con l n) = Con (none l) (scopeVal st n)
    scope st (Lit l i) = Lit (none l) (scope st i)
    scope st (InfixApp l e1 o e2) = InfixApp (none l) (scope st e1) (scope st o) (scope st e2)
    scope st (App l e1 e2) = App (none l) (scope st e1) (scope st e2)
    scope st (NegApp l e) = NegApp (none l) (scope st e)
    scope st (Lambda l ps e) = flip evalState st $ Lambda (none l) <$> scopePats ps <*> scopeM e
    scope st (Let l b e) = flip evalState st $ Let (none l) <$> scopeBinds b <*> scopeM e
    scope st (If l e1 e2 e3) = If (none l) (scope st e1) (scope st e2) (scope st e3)
    scope st (Case l e as) = Case (none l) (scope st e) (fmap (scope st) as)
    scope st (Do l ss) = flip evalState st $ Do (none l) <$> scopeStmts ss
    -- MDo
    scope st (Tuple l es) = Tuple (none l) (fmap (scope st) es)
    scope st (TupleSection l es) = TupleSection (none l) (fmap (fmap (scope st)) es)
    scope st (List l es) = List (none l) (fmap (scope st) es)
    scope st (Paren l e) = Paren (none l) (scope st e)
    scope st (LeftSection l e o) = LeftSection (none l) (scope st e) (scope st o)
    scope st (RightSection l o e) = RightSection (none l) (scope st o) (scope st e)
    scope st (RecConstr l n fs) = RecConstr (none l) (scopeVal st n) (fmap (scope st) fs)
    scope st (RecUpdate l e fs) = RecUpdate (none l) (scope st e) (fmap (scope st) fs)
    scope st (EnumFrom l e) = EnumFrom (none l) (scope st e)
    scope st (EnumFromTo l e1 e2) = EnumFromTo (none l) (scope st e1) (scope st e2)
    scope st (EnumFromThen l e1 e2) = EnumFromThen (none l) (scope st e1) (scope st e2)
    scope st (EnumFromThenTo l e1 e2 e3) = EnumFromThenTo (none l) (scope st e1) (scope st e2) (scope st e3)
    scope st (ListComp l e ss) = flip evalState st $ do
      ss' <- scopeQualStmts ss
      ListComp (none l) <$> scopeM e <*> pure ss'
    -- ParComp
    scope st (ExpTypeSig l e t) = ExpTypeSig (none l) (scope st e) (scopeType st t)
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
    scope _ e = unimplemented $ "scope: " ++ take 30 (prettyPrint e)

scopeQualStmts :: [QualStmt SrcSpan] -> State SymbolTable [QualStmt (Scoped SrcSpan)]
scopeQualStmts = unimplemented "scopeQualStmts"

instance ScopeCheck QOp where
    scope st (QVarOp l n) = QVarOp (none l) (scopeVal st n)
    scope st (QConOp l n) = QConOp (none l) (scopeVal st n)

instance ScopeCheck Alt
instance ScopeCheck FieldUpdate

instance ScopeCheck Binds where
  scope st bnds = evalState (scopeBinds bnds) st
