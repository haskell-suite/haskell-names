module Language.Haskell.Modules.Scope(
    scopeAnalysis,
    Scoped(..), getOriginalName,
    scopeModule,
    getModules,
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
import Language.Haskell.Modules.ModuleSummary
import Language.Haskell.Modules.Recursive
import Language.Haskell.Modules.ScopeMonad
import Language.Haskell.Modules.SymbolTable
import Language.Haskell.Modules.SyntaxUtils


getOriginalName :: (Annotated a) => a (Scoped l) -> Maybe (QName SrcLoc)
getOriginalName x =
    case ann x of
    Global { sOrginalName = n } -> Just n
    _ -> Nothing

scopeAnalysis
  :: Monad m
  => (ModuleName () -> m ModuleSummary)
  -> Flags
  -> [Module SrcSpan]
  -> m ([Msg], [Module (Scoped SrcSpan)])
scopeAnalysis getModInfo flags =
  liftM (sortBy (compare `on` msgLoc) *** concat) .
  runS getModInfo flags .
  mapM scopeGroup .
  groupModules True

scopeGroup :: Monad m => [Module SrcSpan] -> S m [Module (Scoped SrcSpan)]
scopeGroup [m] = liftM return $ scopeModule m
scopeGroup _ = unimplemented "mutually recursive modules"

scopeModule :: Monad m => Module SrcSpan -> S m (Module (Scoped SrcSpan))
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
        -- NB: this is not the same as calling getExportSpecList on m',
        -- because if the module doesn't have a module head, the scope
        -- annotations will be wrong.
        exportList = evalState (runScopeM $ mapM scopeR $ getExportSpecList m) st

    -- Remember module for future processing.
    addModuleSymbols mname =<< filterExports exportList syms

    --- XXX Clear symbol table

    return m'

-----------------------------------------------------------------------------

processImports :: Monad m => (SrcInfo l) => l -> [ImportDecl l] -> S m ()
processImports l is = do
    flgs <- getFlags
    let is' = if any ((=~= pm) . importModule) is || not (f_usePrelude flgs) then is else ip : is
        ip = ImportDecl l pm False False Nothing Nothing Nothing
        pm = prelude_mod l
    mapM_ processImport is'

-- Import all identifiers and add them to the global symbol table.
processImport :: Monad m => (SrcInfo l) => ImportDecl l -> S m ()
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

filterValues
  :: (SrcInfo l, Monad m)
  => Maybe (ImportSpecList l) -> SymValueInfos -> S m SymValueInfos
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

filterTypes
  :: (SrcInfo l, Monad m)
  => Maybe (ImportSpecList l) -> SymTypeInfos -> S m SymTypeInfos
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

dupCheck
  :: (SrcInfo l, Monad m)
  => String -> [Name l] -> S m ()
dupCheck msg = mapM_ report . filter ((> 1) . length) . groupBy ((==) `on` dropAnn) . sortBy (compare `on` dropAnn)
  where report (n:ns) = scopeMsg $ msgError (getPointLoc $ ann n) ("Duplicate " ++ msg ++ "s") $ map msgArgLoc ns
        report [] = internalError "dupCheck"

strayCheck
  :: (SrcInfo l', Monad m)
  => String -> [Name l] -> [Name l'] -> S m ()
strayCheck msg as = mapM_ check1
  where s = S.fromList $ map dropAnn as
        check1 n = unless (dropAnn n `S.member` s) $ scopeMsg $ msgError (getPointLoc $ ann n) ("Stray " ++ msg) [msgArg n]



-----------------------------------------------------------------------------

getScopeErrors :: [Module (Scoped SrcSpan)] -> [Msg]
getScopeErrors ms = [ m | ScopeError _ m <- universeBi ms :: [Scoped SrcSpan] ]

-----------------------------------------------------------------------------

