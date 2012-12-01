module Language.Haskell.Modules.ModuleSymbols where

import Data.List
import Data.Maybe
import Language.Haskell.Exts.Annotated

import Language.Haskell.Modules.Types
import Language.Haskell.Modules.SyntaxUtils

-- Extract names that get bound by a top level declaration.
getTopDeclSymbols
  :: ModuleName l
  -> Decl SrcSpan
  -> [Either (SymValueInfo (QName SrcLoc)) (SymTypeInfo (QName SrcLoc))]
getTopDeclSymbols mdl d =
  case d of
    TypeDecl _ dh _ ->
        let tn = hname dh
        in  [ Right (SymType        { st_origName = qname tn, st_fixity = Nothing })]
    TypeFamDecl _ dh _ ->
        let tn = hname dh
        in  [ Right (SymTypeFam     { st_origName = qname tn, st_fixity = Nothing })]
    DataDecl _ _ _ dh _ _ ->
        let dn = hname dh
            dq = qname dn
            (cs, fs) = partition isCon $ getBound d
            as = cs ++ nub fs  -- Ignore multiple selectors for now
        in    Right (SymData        { st_origName = dq,       st_fixity = Nothing }) :
            [ if isCon cn then
              Left  (SymConstructor { sv_origName = qname cn, sv_fixity = Nothing, sv_typeName = dq }) else
              Left  (SymSelector    { sv_origName = qname cn, sv_fixity = Nothing, sv_typeName = dq })
            | cn <- as ]
    GDataDecl _ _ _ dh _ _ _ ->
        let dn = hname dh
            cq = qname dn
            (cs, fs) = partition isCon $ getBound d
            as = cs ++ nub fs  -- Ignore multiple selectors for now
        in    Right (SymData        { st_origName = cq,       st_fixity = Nothing }) :
            [ if isCon cn then
              Left  (SymConstructor { sv_origName = qname cn, sv_fixity = Nothing, sv_typeName = cq }) else
              Left  (SymSelector    { sv_origName = qname cn, sv_fixity = Nothing, sv_typeName = cq })
            | cn <- as ]
    ClassDecl _ _ _ _ mds ->
        let ms = getBound d
            cn = getDeclHeadName d
            cq = qname cn
            cdecls = fromMaybe [] mds
        in    Right (SymClass       { st_origName = cq,       st_fixity = Nothing }) :
            [ Right (SymTypeFam     { st_origName = qname dn, st_fixity = Nothing }) | ClsTyFam   _   dh _ <- cdecls, let dn = hname dh ] ++
            [ Right (SymDataFam     { st_origName = qname tn, st_fixity = Nothing }) | ClsDataFam _ _ dh _ <- cdecls, let tn = hname dh ] ++
            [ Left  (SymMethod      { sv_origName = qname mn, sv_fixity = Nothing, sv_className = cq }) | mn <- ms ]
--    TypeSig _ ns _ ->
--            [ Left  (SymValue       { sv_origName = qname vn, sv_fixity = Nothing } | vn <- ns ]
    FunBind _ ms ->
        let vn : _ = getBound ms
        in  [ Left  (SymValue       { sv_origName = qname vn, sv_fixity = Nothing }) ]
    PatBind _ p _ _ _ ->
            [ Left  (SymValue       { sv_origName = qname vn, sv_fixity = Nothing }) | vn <- getBound p ]
    ForImp _ _ _ _ fn _ ->
            [ Left  (SymValue       { sv_origName = qname fn, sv_fixity = Nothing }) ]
    _ ->    []
  where ModuleName _ smdl = mdl
        qname n = setAnn (getPointLoc $ ann n) $ Qual undefined (ModuleName undefined smdl) n
        hname = fst . splitDeclHead
