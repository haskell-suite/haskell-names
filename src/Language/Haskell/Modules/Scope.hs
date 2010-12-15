{-# LANGUAGE PatternGuards, DeriveDataTypeable #-}
module Language.Haskell.Modules.Scope(scopeAnalysis, Scoped(..), getScopeErrors) where
import Control.Arrow((***))
import Control.Monad
import Data.Data
import Data.Either
import Data.Function
import Data.Generics.PlateData
import Data.List
import Data.Maybe
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
    = Global     { sLoc :: l, sDefLoc :: l, sOrginalName :: String }
    | Local      { sLoc :: l, sDefLoc :: l }
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

scopeAnalysis :: Flags -> ModuleSet -> ([Msg], [Module (Scoped SrcSpan)])
scopeAnalysis flags = (id *** concat) . runS flags . mapM scope . groupModules True

scope :: ModuleSet -> S [Module (Scoped SrcSpan)]
scope [Left s] = fmap return $ scopeSummary s
scope [Right m] = fmap return $ scopeModule m
scope _ = unimplemented "mutually recursive modules"

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
    dupCheck "type definition" tns                       -- Check for duplicate types/classes
    dupCheck "value definition" vns                      -- Check for duplicate values
    strayCheck "type signature" vns' sigs                -- Check for stray type signatures
    dupCheck "type signature" sigs                       -- Check for duplicate type signatures
    strayCheck "infix declaration" ans fixes             -- Check for stray infix declarations
    dupCheck "infix declaration" fixes                   -- Check for duplicate infix declarations

    processImports (ann m) (getImports m)

    -- Remember module for future processing.
    addModuleSymbols mname =<< filterExports (getExportSpecList m) syms

    return $ fmap None m

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
    (vs, ts) <- getModuleSymbols $ dropAnn im
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
             [ cName cn | IThingWith _ _ cns <- ss, cn <- cns ]
        cName (VarName _ n) = n
        cName (ConName _ n) = n
        impValues = S.fromList $ map dropAnn ns
        impTypes = S.fromList [ dropAnn n | IThingAll _ n <- ss ]
        vns = S.fromList $ map (dropAnn . qNameToName . sv_origName) vs
        chk n | not (S.member (dropAnn n) vns) = scopeMsg $ msgError (getPointLoc $ ann n) "Module does not export value " [msgArg n]
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
        chk n | not (S.member (dropAnn n) tns) = scopeMsg $ msgError (getPointLoc $ ann n) "Module does not export type/class " [msgArg n]
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

--undefinedXXX = error "undefinedXXX"

dupCheck :: (SrcInfo l) => String -> [Name l] -> S ()
dupCheck msg = mapM_ report . filter ((> 1) . length) . groupBy ((==) `on` dropAnn) . sortBy (compare `on` dropAnn) 
  where report (n:ns) = scopeMsg $ msgError (getPointLoc $ ann n) ("Duplicate " ++ msg ++ "s") $ map msgArgLoc ns
        report [] = internalError "dupCheck"

strayCheck :: (SrcInfo l') => String -> [Name l] -> [Name l'] -> S ()
strayCheck msg as = mapM_ check1
  where s = S.fromList $ map dropAnn as
        check1 n = unless (dropAnn n `S.member` s) $ scopeMsg $ msgError (getPointLoc $ ann n) ("Stray " ++ msg) [msgArg n]

{-
doImport :: SymbolTable -> ImportDecl SrcSpan -> S SymbolTable
doImport st i = do
    return st
-}

filterExports :: Maybe (ExportSpecList l) -> Symbols-> S Symbols
filterExports _ l = return l
{-
filterExports Nothing l = return l
filterExports (Just (ExportSpecList _ specs)) (ts, vs) = (filter expType ts, filter expValue vs)
  where expValue (SymConstructor { sv_typeName = qn }) | unQual qn `elem` allTys = True
        expValue (SymMethod { sv_className = qn }) | unQual qn `elem` allTys = True
        expValue (SymSelector { sv_typeName = qn }) | unQual qn `elem` allTys = True
        expValue i = unQual (sv_origName i) `elem` vars
        vars = [ dropAnn n | EVar _ n <- specs ] ++
               [ UnQual () $ dropAnn $ unCName cn | EThingWith _ _ cns <- specs, cn <- cns ]
        expType  i = unQual (st_origName i) `elem` tys
        tys = [ dropAnn n | EAbs _ n <- specs ] ++ allTys ++
              [ dropAnn n | EThingWith _ n _ <- specs ]
        allTys = [ dropAnn n | EThingAll _ n <- specs ]
-}

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
            cs = getBound d
        in    Right (SymData        { st_origName = dq,       st_fixity = fixity dn }) :
            [ if isCon cn then
              Left  (SymConstructor { sv_origName = qname cn, sv_fixity = fixity cn, sv_typeName = dropAnn dq }) else
              Left  (SymSelector    { sv_origName = qname cn, sv_fixity = fixity cn, sv_typeName = dropAnn dq })
            | cn <- cs ]
    GDataDecl _ _ _ dh _ _ _ ->
        let dn = hname dh
            cq = qname dn
            cs = getBound d
        in    Right (SymData        { st_origName = cq,       st_fixity = fixity dn }) :
            [ if isCon cn then
              Left  (SymConstructor { sv_origName = qname cn, sv_fixity = fixity cn, sv_typeName = dropAnn cq }) else
              Left  (SymSelector    { sv_origName = qname cn, sv_fixity = fixity cn, sv_typeName = dropAnn cq })
            | cn <- cs ]
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
