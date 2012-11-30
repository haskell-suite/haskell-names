{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Modules.Types where

import Language.Haskell.Exts.Annotated
import Language.Haskell.Modules.Error
import Data.Typeable
import Data.Data

type SymFixity = (Assoc (), Int)

data SymValueInfo name
    = SymValue       { sv_origName :: name, sv_fixity :: Maybe SymFixity }
    | SymMethod      { sv_origName :: name, sv_fixity :: Maybe SymFixity, sv_className :: name }
    | SymSelector    { sv_origName :: name, sv_fixity :: Maybe SymFixity, sv_typeName :: name }
    | SymConstructor { sv_origName :: name, sv_fixity :: Maybe SymFixity, sv_typeName :: name }
    deriving (Eq, Show, Data, Typeable)

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
    deriving (Eq, Show, Data, Typeable)

type SymTypeInfos n = [SymTypeInfo n]
type NameS = String
type ModuleNameS = String

-- | Possibly qualified name. If the name is not qualified,
-- 'ModuleNameS' is the empty string.
data GName = GName ModuleNameS NameS
  deriving (Eq, Ord, Show, Data, Typeable)
-- | Qualified name, where 'ModuleNameS' points to the module where the
-- name was originally defined. The module part is never empty.
type OrigName = GName

data Scoped l
    = GlobalValue { sLoc :: l, sVInfo :: SymValueInfo OrigName }
    | GlobalType  { sLoc :: l, sTInfo :: SymTypeInfo  OrigName }
    | LocalValue  { sLoc :: l, sDefLoc :: SrcLoc }
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
