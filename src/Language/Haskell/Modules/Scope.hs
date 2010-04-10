{-# LANGUAGE PatternGuards #-}
module Language.Haskell.Modules.Scope(scopeAnalysis, Scoped(..)) where
import Control.Arrow((***))
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Data.Tuple.Select
import qualified Language.Haskell.Exts.Fixity as F
import qualified Language.Haskell.Exts.Syntax as S
import Language.Haskell.Exts.Annotated hiding (fixities)

import Language.Haskell.Modules.Error
import Language.Haskell.Modules.MonadModule
import Language.Haskell.Modules.Recursive
import Language.Haskell.Modules.ResolveMonad(ModuleSet)
import Language.Haskell.Modules.ScopeMonad
import Language.Haskell.Modules.SymbolTable
import Language.Haskell.Modules.SyntaxUtils

data Scoped l
    = Global { sLoc :: l, sOrginalName :: String }
    | Local { sLoc :: l }
    | None { sLoc :: l }
    | ScopeError { sLoc :: l, serr :: Msg }
    deriving (Show)

instance (SrcInfo l) => SrcInfo (Scoped l) where
    toSrcInfo _l1 _ss _l2 = unimplemented "toSrcInfo Scoped"
    fromSrcInfo _si = unimplemented "fromSrcInfo Scoped"
    getPointLoc = getPointLoc . sLoc
    fileName = fileName . sLoc
    startLine = startLine . sLoc
    startColumn = startColumn . sLoc

scopeAnalysis :: ModuleSet -> ([Msg], [Module (Scoped SrcSpan)])
scopeAnalysis = (id *** concat) . runS . mapM scope . groupModules True

scope :: ModuleSet -> S [Module (Scoped SrcSpan)]
scope [Left s] = scopeSummary s
scope [Right m] = scopeModule m
scope _ = unimplemented "mutually recursive modules"

scopeSummary :: ModuleSummary -> S [Module (Scoped SrcSpan)]
scopeSummary s = do
    addSymbolList (ModuleName () (m_moduleName s)) . summaryToSyms $ s
    return []

summaryToSyms :: ModuleSummary -> SymbolList
summaryToSyms s = SymbolList {
    sl_types  = [ (tn, mn, ps, c) | (tn, ps, c) <- m_types s ],
    sl_values = [ (vn, mn) | vn <- m_values s ] ++
                [ (vn, mn) | (_, ps, _) <- m_types s, vn <- ps ],
    sl_fixities = m_fixities s
    }
  where mn = m_moduleName s

scopeModule :: Module SrcSpan -> S [Module (Scoped SrcSpan)]
scopeModule m = do
    let mname = getModuleName m
        decls = getModuleDecls m
        (ts, vns) = (concat *** concat) $ unzip $ map addDecl decls
        tns = map sel1 ts
        vns' = filter (not . (`S.member` cs) . dropAnn) vns
               where cs = S.fromList $ [ dropAnn n | (_, ns, _) <- ts, n <- ns ]
        sigs = [ n | TypeSig _ ns _ <- decls, n <- ns ]
        fixes = [ opName o | InfixDecl _ _ _ ops <- decls, o <- ops ]
        ans = tns ++ vns
    dupCheck "type definition" tns                       -- Check for duplicate types/classes
    dupCheck "value definition" vns                      -- Check for duplicate values
    strayCheck "type signature" vns' sigs                -- Check for stray type signatures
    dupCheck "type signature" sigs                       -- Check for duplicate type signatures
    strayCheck "infix declaration" ans fixes             -- Check for stray infix declarations
    dupCheck "infix declaration" fixes                   -- Check for duplicate infix declarations

    let ModuleName _ smname = mname
        types = [ (nameToString n, smname, map nameToString ps, cls) | (n, ps, cls) <- ts ]
        values = [ (nameToString n, smname) | n <- vns ]
        fixities = [ F.Fixity (getAssoc a) (fromMaybe 9 mp) (sop $ opName o) | InfixDecl _ a mp ops <- decls, o <- ops ]
                   where getAssoc (AssocNone  _) = S.AssocNone
                         getAssoc (AssocLeft  _) = S.AssocLeft
                         getAssoc (AssocRight _) = S.AssocRight
                         sop n@(Ident  _ s) = (if isCon n then S.ConOp else S.VarOp) $ S.Ident s
                         sop n@(Symbol _ s) = (if isCon n then S.ConOp else S.VarOp) $ S.Symbol s
        sl = SymbolList { sl_types = types, sl_values = values, sl_fixities = fixities }

--    symTab <- foldM doImport {- symEmpty -} undefinedXXX $ getImportDecls m

    let m' = m  -- scope check all ids

    sl' <- filterExports (getExportSpecList m') sl  -- XXX wrong, needs to look at all symbols in scope
    addSymbolList (dropAnn mname) sl'

    return undefined -- [m']

--undefinedXXX = error "undefinedXXX"

dupCheck :: String -> [Name SrcSpan] -> S ()
dupCheck msg = mapM_ report . filter ((> 1) . length) . groupBy ((==) `on` dropAnn) . sortBy (compare `on` dropAnn) 
  where report (n:ns) = scopeMsg $ msgError (getPointLoc $ ann n) ("Duplicate " ++ msg ++ "s") $ map MsgArg ns
        report [] = internalError "dupCheck"

strayCheck :: String -> [Name l] -> [Name SrcSpan] -> S ()
strayCheck msg as = mapM_ check1
  where s = S.fromList $ map dropAnn as
        check1 n = unless (dropAnn n `S.member` s) $ scopeMsg $ msgError (getPointLoc $ ann n) ("Stray " ++ msg) [msgArg n]

{-
doImport :: SymbolTable -> ImportDecl SrcSpan -> S SymbolTable
doImport st i = do
    return st
-}

filterExports :: Maybe (ExportSpecList l) -> SymbolList -> S SymbolList
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
addDecl :: Decl SrcSpan -> ([(Name SrcSpan, [Name SrcSpan], Bool)], [Name SrcSpan])
addDecl d@(ClassDecl _ _ _ _ mds) =
                let cs = getBound d
                in  ((getDeclHeadName d, cs, True):
                        map (\ t -> (t, [], False)) (concatMap getTypes $ fromMaybe [] mds)
                    , cs
                    )
  where getTypes (ClsDataFam _ _ dh _) = [fst $ splitDeclHead dh]
        getTypes (ClsTyFam _ dh _) = [fst $ splitDeclHead dh]
        getTypes _ = []
addDecl d | isTypeDecl d =
                let cs = getBound d
                in  ([(getDeclHeadName d, cs, False)]
                    , cs
                    )
            | isValue d = ([], getBound d)
            | otherwise = ([], [])
  where isValue (DataInsDecl{}) = True
        isValue (GDataInsDecl{}) = True
        isValue (FunBind{}) = True
        isValue (PatBind{}) = True
        isValue (ForImp{}) = True
        isValue _ = False
