{-# LANGUAGE PatternGuards #-}
module Language.Haskell.Modules.Scope(scopeAnalysis, Scoped(..)) where
import Control.Arrow((***))
import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.Error
import Language.Haskell.Modules.MonadModule
import Language.Haskell.Modules.Recursive
import Language.Haskell.Modules.ResolveMonad(ModuleSet)
import Language.Haskell.Modules.ScopeMonad
import Language.Haskell.Modules.SymbolTable
import Language.Haskell.Modules.SyntaxUtils

data Scoped l
    = Global { sloc :: l, smodule :: ModuleName () }
    | Local { sloc :: l }
    | None { sloc :: l }
    | ScopeError { sloc :: l }
    deriving (Show, Eq)

instance (SrcInfo l) => SrcInfo (Scoped l) where
    toSrcInfo _l1 _ss _l2 = unimplemented "toSrcInfo Scoped"
    fromSrcInfo _si = unimplemented "fromSrcInfo Scoped"
    getPointLoc = getPointLoc . sloc
    fileName = fileName . sloc
    startLine = startLine . sloc
    startColumn = startColumn . sloc

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
    sl_types  = [ (tn, mn, ps, fixity tn) | (tn, ps) <- m_types s ],
    sl_values = [ (vn, mn, fixity vn) | vn <- m_values s ] ++
                [ (vn, mn, fixity vn) | (_, ps) <- m_types s, vn <- ps ]
    }
  where mn = m_moduleName s
        fixity i = lookup i fixes
        fixes = m_fixities s

scopeModule :: Module SrcSpan -> S [Module (Scoped SrcSpan)]
scopeModule m = do
    let mname = getModuleName m
        decls = getModuleDecls m
        (ts, vs) = (concat *** concat) $ unzip $ map (addDecl mname) decls
        tns = map (\ (_, n, _, _) -> n) ts
        vns = map (\ (_, n, _) -> n) vs
        vns' = [ n | (_, n, VNormal) <- vs ]
        sigs = [ n | TypeSig _ ns _ <- decls, n <- ns ]
        fixes = [ opName o | InfixDecl _ _ _ ops <- decls, o <- ops ]
        ans = tns ++ vns
    dupCheck "type definition" tns                       -- Check for duplicate types/classes
    dupCheck "value definition" vns                      -- Check for duplicate values
    strayCheck "type signature" vns' sigs                -- Check for stray type signatures
    dupCheck "type signature" sigs                       -- Check for duplicate type signatures
    strayCheck "infix declaration" ans fixes             -- Check for stray infix declarations
    dupCheck "infix declaration" fixes                   -- Check for duplicate infix declarations

{-
    let sl = moduleSymList m
    sl' <- filterExports (getExportSpec m) sl
    addSymbolList (getModuleName m) sl'
    -- ...
-}
    return []

dupCheck :: String -> [Name SrcSpan] -> S ()
dupCheck msg = mapM_ report . filter ((> 1) . length) . groupBy ((==) `on` dropAnn) . sortBy (compare `on` dropAnn) 
  where report (n:ns) = scopeMsg $ msgError (getPointLoc $ ann n) ("Duplicate " ++ msg ++ "s") $ map MsgArg ns
        report [] = internalError "dupCheck"

strayCheck :: String -> [Name l] -> [Name SrcSpan] -> S ()
strayCheck msg as = mapM_ check1
  where s = S.fromList $ map dropAnn as
        check1 n = unless (dropAnn n `S.member` s) $ scopeMsg $ msgError (getPointLoc $ ann n) ("Stray " ++ msg) [msgArg n]

--filterExports :: Maybe (ExportSpecList l) -> SymbolList -> S SymbolList
--filterExports Nothing l = return l

type TypeInfo l = (ModuleName l, Name l, [Name l], Bool)
type ValueInfo l = (ModuleName l, Name l, ValueKind)

data ValueKind = VNormal | VSelector | VMethod | VConstructor | VImported
    deriving (Show, Eq, Ord)

-- Extract names that get bound by a top level declaration.
addDecl :: ModuleName SrcSpan -> Decl SrcSpan -> ([TypeInfo SrcSpan], [ValueInfo SrcSpan])
addDecl m d@(ClassDecl _ _ _ _ mds) = 
                let cs = getBound d
                in  ((m, getDeclHeadName d, cs, True):
                        map (\ t -> (m, t, [], False)) (concatMap getTypes $ fromMaybe [] mds)
                    , zip3 (repeat m) cs (repeat VMethod)
                    )
  where getTypes (ClsDataFam _ _ dh _) = [fst $ splitDeclHead dh]
        getTypes (ClsTyFam _ dh _) = [fst $ splitDeclHead dh]
        getTypes _ = []
addDecl m d | isTypeDecl d =
                let cs = getBound d
                in  ([(m, getDeclHeadName d, cs, False)]
                    , zip3 (repeat m) cs (map (\ n -> if isCon n then VConstructor else VSelector) cs)
                    )
            | Just k <- getValue d = ([], zip3 (repeat m) (getBound d) (repeat k))
            | otherwise = ([], [])
  where getValue (DataInsDecl{}) = Just VConstructor
        getValue (GDataInsDecl{}) = Just VConstructor
        getValue (FunBind{}) = Just VNormal
        getValue (PatBind{}) = Just VNormal
        getValue (ForImp{}) = Just VImported
        getValue _ = Nothing
