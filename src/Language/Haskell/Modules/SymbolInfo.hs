module Language.Haskell.Modules.SymbolInfo where

import qualified Language.Haskell.Exts.Syntax as S

type SymFixity = (S.Assoc, Int)

data SymValueInfo name
    = SymValue       { sv_origName :: name, sv_fixity :: Maybe SymFixity }
    | SymMethod      { sv_origName :: name, sv_fixity :: Maybe SymFixity, sv_className :: name }
    | SymSelector    { sv_origName :: name, sv_fixity :: Maybe SymFixity, sv_typeName :: name }
    | SymConstructor { sv_origName :: name, sv_fixity :: Maybe SymFixity, sv_typeName :: name }
    deriving (Eq, Show)

type SymValueInfos n = [SymValueInfo n]

sv_parent :: SymValueInfo n -> Maybe n
sv_parent (SymSelector { sv_typeName = n }) = Just n
sv_parent (SymConstructor { sv_typeName = n }) = Just n
sv_parent (SymMethod { sv_className = n }) = Just n
sv_parent _ = Nothing

data SymTypeInfo name
    = SymType        { st_origName :: name, st_fixity :: Maybe SymFixity }
    | SymData        { st_origName :: name, st_fixity :: Maybe SymFixity }
    | SymTypeFam     { st_origName :: name, st_fixity :: Maybe SymFixity }
    | SymDataFam     { st_origName :: name, st_fixity :: Maybe SymFixity }
    | SymClass       { st_origName :: name, st_fixity :: Maybe SymFixity }
    deriving (Eq, Show)

type SymTypeInfos n = [SymTypeInfo n]
