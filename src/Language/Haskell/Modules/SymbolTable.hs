module Language.Haskell.Modules.SymbolTable(
    SymbolTable, symEmpty, symShow, --symMerge,
    SymFixity,
    SymValueInfo(..), sv_parent, SymValueInfos, symValueLookup, symValueAdd,
    SymTypeInfo(..), SymTypeInfos, symTypeLookup, symTypeAdd,
    Symbols, symInfos, -- symMake, --symAssocs,
--    QU, toQU
    ) where
import qualified Data.Map as M
--import Control.Arrow((&&&), first)
import Data.List(union)
import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Exts.Syntax as S
--import qualified Language.Haskell.Exts.Fixity as S

--import Language.Haskell.Modules.MonadModule(ModuleNameS, TypeName, ValueName, WithOrigName)

import Language.Haskell.Modules.SyntaxUtils(nameToString, specialConToString)

data SymbolTable = T (M.Map QU SymValueInfo)
                     (M.Map QU SymTypeInfo)
    deriving (Show)

type SymFixity = (S.Assoc, Int)

-- Use this type for key to speed up lookup and avoid all the special cases in QName.
data QU
    = Q String String -- qualified name
    | U String        -- unqualified name
    deriving (Eq, Ord)
instance Show QU where
    show (Q m i) = m ++ "." ++ i
    show (U i) = i

toQU :: QName l -> QU
toQU (UnQual _ n) = U (nameToString n)
toQU (Qual _ (ModuleName _ m) n) = Q m (nameToString n)
toQU (Special _ s) = Q p (specialConToString s)  where ModuleName _ p = prelude_mod ()

--fromQU :: QU -> QName ()
--fromQU (U n) = UnQual () $ Ident () n
--fromQU (Q m n) = Qual () (ModuleName () m) $ Ident () n

-- The SrcLoc in the origName is the location of the definition.

data SymValueInfo
    = SymValue       { sv_origName :: QName SrcLoc, sv_fixity :: Maybe SymFixity }
    | SymMethod      { sv_origName :: QName SrcLoc, sv_fixity :: Maybe SymFixity, sv_className :: QName () }
    | SymSelector    { sv_origName :: QName SrcLoc, sv_fixity :: Maybe SymFixity, sv_typeName :: QName () }
    | SymForeign     { sv_origName :: QName SrcLoc, sv_fixity :: Maybe SymFixity }
    | SymConstructor { sv_origName :: QName SrcLoc, sv_fixity :: Maybe SymFixity, sv_typeName :: QName () }
    | SymVClash      { sv_origName :: QName SrcLoc, sv_fixity :: Maybe SymFixity, sv_clash :: [QName SrcLoc] }
    deriving (Eq, Show)

sv_parent :: SymValueInfo -> Maybe (QName ())
sv_parent (SymSelector { sv_typeName = n }) = Just n
sv_parent (SymConstructor { sv_typeName = n }) = Just n
sv_parent (SymMethod { sv_className = n }) = Just n
sv_parent _ = Nothing

type SymValueInfos = [SymValueInfo]

data SymTypeInfo
    = SymType        { st_origName :: QName SrcLoc, st_fixity :: Maybe SymFixity }
    | SymData        { st_origName :: QName SrcLoc, st_fixity :: Maybe SymFixity }
    | SymTypeFam     { st_origName :: QName SrcLoc, st_fixity :: Maybe SymFixity }
    | SymDataFam     { st_origName :: QName SrcLoc, st_fixity :: Maybe SymFixity }
    | SymClass       { st_origName :: QName SrcLoc, st_fixity :: Maybe SymFixity }
    | SymTClash      { st_origName :: QName SrcLoc, st_fixity :: Maybe SymFixity, st_clash :: [QName SrcLoc] }
    deriving (Eq, Show)

type SymTypeInfos = [SymTypeInfo]

type Symbols = (SymValueInfos, SymTypeInfos)

symEmpty :: SymbolTable
symEmpty = T M.empty M.empty

--symMerge :: SymbolTable -> SymbolTable -> SymbolTable
--symMerge (T vs ts) (T vs' ts') = T (M.union vs vs') (M.union ts ts')

symValueLookup :: QName l -> SymbolTable -> Maybe SymValueInfo
symValueLookup qn (T vs _) = M.lookup (toQU qn) vs

symValueAdd :: QName l -> SymValueInfo -> SymbolTable -> SymbolTable
symValueAdd qn i (T vs ts) = T (M.insertWith combineValue (toQU qn) i vs) ts

symTypeLookup :: QName l -> SymbolTable -> Maybe SymTypeInfo
symTypeLookup qn (T _ ts) = M.lookup (toQU qn) ts

symTypeAdd :: QName l -> SymTypeInfo -> SymbolTable -> SymbolTable
symTypeAdd qn i (T vs ts) = T vs (M.insertWith combineType (toQU qn) i ts)

symShow :: SymbolTable -> String
symShow (T vs ts) = unlines $ map (\ (i, _si) -> show i) (M.toList vs)
                           ++ map (\ (i, _si) -> show i) (M.toList ts)

symInfos :: SymbolTable -> Symbols
symInfos (T vs ts) = (M.elems vs, M.elems ts)

--symAssocs :: SymbolTable -> ([(QName (), SymTypeInfo)], [(QName (), SymValueInfo)])
--symAssocs (T vs ts) = (map (first . fromQU) $ M.assocs ts, map (first . fromQU) $ M.assocs vs)

--symMake :: SymInfos -> SymbolTable
--symMake (ts, vs) = T (M.fromListWith combineValue $ map (toQU . sv_origName &&& id) vs) (M.fromListWith combineType $ map (toQU . st_origName &&& id) ts)

combineValue :: SymValueInfo -> SymValueInfo -> SymValueInfo
combineValue s1 (SymVClash o f ns) = SymVClash o f (union [sv_origName s1] ns)
combineValue s1 s2 | n1 =~= n2 = s1
                   | otherwise = SymVClash (sv_origName s1) (sv_fixity s1) [n1, n2]
  where n1 = sv_origName s1
        n2 = sv_origName s2

combineType :: SymTypeInfo -> SymTypeInfo -> SymTypeInfo
combineType s1 (SymTClash o f ns) = SymTClash o f (union [st_origName s1] ns)
combineType s1 s2 | n1 =~= n2 = s1
                  | otherwise = SymTClash (st_origName s1) (st_fixity s1) [n1, n2]
  where n1 = st_origName s1
        n2 = st_origName s2
