module Language.Haskell.Names.ModuleSymbols
  ( moduleSymbols
  , moduleTable
  )
  where

import Data.List
import Data.Maybe
import Data.Either
import Data.Lens.Common
import Data.Monoid
import Data.Data
import qualified Data.Set as Set
import Language.Haskell.Exts.Annotated

import Language.Haskell.Names.Types
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import Language.Haskell.Names.SyntaxUtils
import Language.Haskell.Names.ScopeUtils
import Language.Haskell.Names.GetBound

moduleTable :: (Eq l, Data l) => Module l -> Global.Table
moduleTable m =
  computeSymbolTable False (getModuleName m) (moduleSymbols m)

moduleSymbols :: (Eq l, Data l) => Module l -> Symbols
moduleSymbols m =
  let (vs,ts) =
        partitionEithers $
          concatMap
            (getTopDeclSymbols $ getModuleName m)
            (getModuleDecls m)
  in
    setL valSyms (Set.fromList vs) $
    setL tySyms  (Set.fromList ts) mempty

-- Extract names that get bound by a top level declaration.
getTopDeclSymbols
  :: (Eq l, Data l)
  => ModuleName l
  -> Decl l
  -> [Either (SymValueInfo OrigName) (SymTypeInfo OrigName)]
getTopDeclSymbols mdl d =
  map (either (Left . fmap toOrig) (Right . fmap toOrig)) $
  case d of
    TypeDecl _ dh _ ->
      let tn = hname dh
      in  [ Right (SymType        { st_origName = tn, st_fixity = Nothing })]

    TypeFamDecl _ dh _ ->
      let tn = hname dh
      in  [ Right (SymTypeFam     { st_origName = tn, st_fixity = Nothing })]

    DataDecl _ dataOrNew _ dh _ _ ->
      let
        dq = hname dh
        (cs, fs) = partition isCon $ getBound d
        as = cs ++ nub fs  -- Ignore multiple selectors for now
      in
        Right (dataOrNewCon dataOrNew dq Nothing) :
        [ if isCon cn then
          Left  (SymConstructor { sv_origName = qname cn, sv_fixity = Nothing, sv_typeName = dq }) else
          Left  (SymSelector    { sv_origName = qname cn, sv_fixity = Nothing, sv_typeName = dq })
        | cn <- as ]

    GDataDecl _ dataOrNew _ dh _ _ _ ->
      let
        dq = hname dh
        (cs, fs) = partition isCon $ getBound d
        as = cs ++ nub fs  -- Ignore multiple selectors for now
      in
        Right (dataOrNewCon dataOrNew dq Nothing) :
        [ if isCon cn then
          Left  (SymConstructor { sv_origName = qname cn, sv_fixity = Nothing, sv_typeName = dq }) else
          Left  (SymSelector    { sv_origName = qname cn, sv_fixity = Nothing, sv_typeName = dq })
        | cn <- as ]

    ClassDecl _ _ dh _ mds ->
      let
        ms = getBound d
        cq = hname dh
        cdecls = fromMaybe [] mds
      in
          Right (SymClass   { st_origName = cq,       st_fixity = Nothing }) :
        [ Right (SymTypeFam { st_origName = hname dh, st_fixity = Nothing }) | ClsTyFam   _   dh _ <- cdecls ] ++
        [ Right (SymDataFam { st_origName = hname dh, st_fixity = Nothing }) | ClsDataFam _ _ dh _ <- cdecls ] ++
        [ Left  (SymMethod  { sv_origName = qname mn, sv_fixity = Nothing, sv_className = cq }) | mn <- ms ]

    FunBind _ ms ->
      let vn : _ = getBound ms
      in  [ Left  (SymValue { sv_origName = qname vn, sv_fixity = Nothing }) ]

    PatBind _ p _ _ _ ->
      [ Left  (SymValue { sv_origName = qname vn, sv_fixity = Nothing }) | vn <- getBound p ]

    ForImp _ _ _ _ fn _ ->
      [ Left  (SymValue { sv_origName = qname fn, sv_fixity = Nothing }) ]

    _ -> []
  where
    ModuleName _ smdl = mdl
    qname = GName smdl . nameToString
    hname = qname . fst . splitDeclHead
    toOrig = OrigName Nothing
    dataOrNewCon dataOrNew = case dataOrNew of DataType {} -> SymData; NewType {} -> SymNewType
