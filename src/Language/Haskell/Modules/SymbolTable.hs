module Language.Haskell.Modules.SymbolTable(
    SymbolTable, symEmpty, symShow, --symMerge,
    SymFixity,
    SymValueInfo(..), sv_parent, SymValueInfos, symValueLookup, symValueAdd,
    SymTypeInfo(..), SymTypeInfos, symTypeLookup, symTypeAdd,
    Symbols, symInfos, symbolsToSumary
    ) where

import Language.Haskell.Modules.ModuleSummary
import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Exts.Syntax as S
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Control.Exception

import Language.Haskell.Modules.SyntaxUtils(nameToString, specialConToString)

data SymbolTable = T (Map.Map QU SymValueInfo)
                     (Map.Map QU SymTypeInfo)
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
symEmpty = T Map.empty Map.empty

--symMerge :: SymbolTable -> SymbolTable -> SymbolTable
--symMerge (T vs ts) (T vs' ts') = T (Map.union vs vs') (Map.union ts ts')

symValueLookup :: QName l -> SymbolTable -> Maybe SymValueInfo
symValueLookup qn (T vs _) = Map.lookup (toQU qn) vs

symValueAdd :: QName l -> SymValueInfo -> SymbolTable -> SymbolTable
symValueAdd qn i (T vs ts) = T (Map.insertWith combineValue (toQU qn) i vs) ts

symTypeLookup :: QName l -> SymbolTable -> Maybe SymTypeInfo
symTypeLookup qn (T _ ts) = Map.lookup (toQU qn) ts

symTypeAdd :: QName l -> SymTypeInfo -> SymbolTable -> SymbolTable
symTypeAdd qn i (T vs ts) = T vs (Map.insertWith combineType (toQU qn) i ts)

symShow :: SymbolTable -> String
symShow (T vs ts) = unlines $ map (\ (i, _si) -> show i) (Map.toList vs)
                           ++ map (\ (i, _si) -> show i) (Map.toList ts)

symInfos :: SymbolTable -> Symbols
symInfos (T vs ts) = (Map.elems vs, Map.elems ts)

--symAssocs :: SymbolTable -> ([(QName (), SymTypeInfo)], [(QName (), SymValueInfo)])
--symAssocs (T vs ts) = (map (first . fromQU) $ Map.assocs ts, map (first . fromQU) $ Map.assocs vs)

--symMake :: SymInfos -> SymbolTable
--symMake (ts, vs) = T (Map.fromListWith combineValue $ map (toQU . sv_origName &&& id) vs) (Map.fromListWith combineType $ map (toQU . st_origName &&& id) ts)

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

-- Conversion from and to ModuleSummary

data E =
  E !(Set.Set ValueName)
    !(Map.Map TypeName ([ValueName], ClassOrType))

instance Monoid E where
  mempty = E mempty mempty
  mappend (E v1 t1) (E v2 t2) =
    E (mappend v1 v2)
      (Map.unionWith
        (\(vs1,k1) (vs2,k2) ->
          assert (k1 == k2) (vs1++vs2, k1))
        t1 t2)

symbolsToSumary :: (ModuleName (), Symbols) -> ModuleSummary
symbolsToSumary (ModuleName _ n, (vs, ts)) =
  ModuleSummary
    { m_moduleName = n
    , m_values = Set.toList vs'
    , m_types = [ (t,v,k) | (t,(v,k)) <- Map.toList ts' ]
    , m_fixities = []
    }
  where
    E vs' ts' =
      foldl' (\a x -> a `mappend` classifyT x) mempty ts `mappend`
      foldl' (\a x -> a `mappend` classifyV x) mempty vs

    classifyT :: SymTypeInfo -> E
    classifyT v =
      case v of
        SymType t _    -> ty t t Type
        SymData t _    -> ty t t Type
        SymTypeFam t _ -> ty t t Type
        SymDataFam t _ -> ty t t Type
        SymClass t _   -> ty t t Class
        SymTClash {}   -> error "Clash"

    classifyV :: SymValueInfo -> E
    classifyV v =
      case v of
        SymValue n _         -> value n
        SymMethod n _ cls    -> ty cls n Class
        SymSelector n _ t    -> ty t n Type
        SymConstructor n _ t -> ty t n Type
        SymVClash {}         -> error "Clash"

    value n = E (Set.singleton $ qNameToOrigName n) mempty
    ty t v k =
      E mempty
        (Map.singleton (qNameToOrigName t)
        ([qNameToOrigName v], k))

qNameToOrigName :: QName a -> OrigName
qNameToOrigName (Qual _ (ModuleName _ m) n) = OrigName m n'
  where
    n' = case n of Ident _ n' -> n'; Symbol _ n' -> n'
qNameToOrigName _ = error "qNameToOrigName"
