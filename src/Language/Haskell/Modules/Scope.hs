{-# LANGUAGE PatternGuards, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
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

import Prelude hiding (mapM)
import Control.Applicative
import Control.Arrow((***))
import Control.Monad hiding (mapM)
import Control.Monad.State hiding (mapM,get,modify)
import qualified Control.Monad.State as State
import Data.Data
import Data.Either
import Data.Function
import Data.Generics.PlateData
import Data.List
import Data.Maybe
import Data.Traversable (mapM)
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
    let m' = evalState (runScopeM $ scopeR m) st

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

-- | Used as a phantom type
data Modify = Modify

-- | The phantom type @i@ can either be left polymoprhic (which means that
-- the monadic action just reads, but does not modify, the symbol table) or
-- instantiated with 'Modify', which means that the action modifies the symbol
-- table.
newtype ScopeM i a = ScopeM { runScopeM :: State SymbolTable a }
    deriving (Functor, Applicative, Monad)

get :: ScopeM i SymbolTable
get = ScopeM State.get

modify :: (SymbolTable -> SymbolTable) -> ScopeM Modify ()
modify f = ScopeM $ State.modify f

-- We have two different classes, ScopeCheckM and ScopeCheckR, to avoid
-- accidentally using scopeR where scopeM is intended (and thus losing scope
-- changes).

class ScopeCheckM a where
    -- | 'scopeM' may modify the symbol table if the node introduces any bindings
    scopeM :: a SrcSpan -> ScopeM Modify (a (Scoped SrcSpan))

class ScopeCheckR a where
    -- | 'scopeR' never modifies the symbol table
    scopeR :: a SrcSpan -> ScopeM i (a (Scoped SrcSpan))

-- | Delimit the scope of changes that are introduced by the action
delimit :: ScopeM i1 a -> ScopeM i2 a
delimit (ScopeM a) = ScopeM $ do st <- State.get; r <- a; State.put st; return r

none :: l -> Scoped l
none = None

binder :: l -> Scoped l
binder = Binder

noScope :: (Annotated a) => a l -> a (Scoped l)
noScope = fmap None

scopeX :: (SymTypeInfo -> Maybe String) -> String -> QName SrcSpan -> ScopeM i (QName (Scoped SrcSpan))
scopeX ok w qn = flip liftM get $ \st ->
    let f l = case symTypeLookup qn st of
              Nothing -> ScopeError l $ msgError (ann qn) ("Undefined " ++ w ++ " identifier") [msgArg qn]
              Just i -> case ok i of
                        Nothing -> Global l (st_origName i)
                        Just s -> ScopeError l $ msgError (ann qn) s [msgArg qn]
    in  fmap f qn

scopeTyCls :: QName SrcSpan -> ScopeM i (QName (Scoped SrcSpan))
scopeTyCls = scopeX (const Nothing) "type/class"

scopeCls :: QName SrcSpan -> ScopeM i (QName (Scoped SrcSpan))
scopeCls = scopeX cls "class"
  where cls SymClass{} = Nothing
        cls _ = Just "Type used as a class"

scopeTy :: QName SrcSpan -> ScopeM i (QName (Scoped SrcSpan))
scopeTy = scopeX cls "type"
  where cls SymClass{} = Just "Class used as a type"
        cls _ = Nothing

scopeTyVar :: Name SrcSpan -> ScopeM i (Name (Scoped SrcSpan))
scopeTyVar n = fixup <$> scopeX cls "type variable" (UnQual (ann n) n)
  where cls SymClass{} = Just "Class used as a type"
        cls _ = Nothing
        fixup = fmap lcl . qNameToName
        lcl (Global l qn) = Local l (ann qn)
        lcl l = l

-- XXX Handle local symbols
scopeVal :: QName SrcSpan -> ScopeM i (QName (Scoped SrcSpan))
scopeVal qn = flip liftM get $ \st ->
    let f l = case symValueLookup qn st of
              Nothing -> ScopeError l $ msgError (ann qn) "Undefined value identifier" [msgArg qn]
              Just i  -> Global l (sv_origName i)
    in  fmap f qn

--- XXX
scopeVar :: Name SrcSpan -> ScopeM i (Name (Scoped SrcSpan))
scopeVar n = flip liftM get $ \st ->
    let f l = case symValueLookup (UnQual (ann n) n) st of
              Nothing -> ScopeError l $ msgError (ann n) "Undefined value identifier" [msgArg n]
              Just i  -> Local l (ann $ sv_origName i)
    in  fmap f n

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
    scopeR (EThingWith _l _qn _cns) = unimplemented "EThingWith"  -- check that cns are exportable
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

addVars :: [Name SrcSpan] -> SymbolTable -> SymbolTable
addVars vs st = foldl' (\st v -> addVar v st) st vs
  where
  addVar v st =
    let qv = UnQual (ann v) v
    in symValueAdd qv (SymValue (getPointLoc <$> qv) Nothing) st

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
    (\p ->       scopePatR p  <* modify (addVars $ getBound p ),
     \ps -> mapM scopePatR ps <* modify (addVars $ getBound ps))
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
    -- MDo
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
      modify $ addVars $ getBound ds
      BDecls (none l) <$> mapM scopeR ds
    scopeM IPBinds {} = unimplemented "scope: IPBinds"
