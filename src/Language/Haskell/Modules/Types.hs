{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Modules.Types where

import Language.Haskell.Exts.Annotated
import Data.Typeable
import Data.Data
import {-# SOURCE #-} qualified Language.Haskell.Modules.GlobalSymbolTable as Global

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

type Symbols n = ([SymValueInfo n], [SymTypeInfo n])

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
    | TypeVar     { sLoc :: l, sDefLoc :: SrcLoc }
    | Binder      { sLoc :: l }
    | Import      { sLoc :: l, importTable :: Global.Table }
    | ImportPart  { sLoc :: l, importSymbols :: Symbols OrigName }
    | Export      { sLoc :: l, exportSymbols :: Symbols OrigName }
    | None        { sLoc :: l }
    | ScopeError  { sLoc :: l, serr :: Error l }
    deriving (Show, Typeable, Data)

data Error l
  = ENotInScope (QName l) -- FIXME annotate with namespace (types/values)
  | EAmbiguous (QName l) [OrigName]
  | ETypeAsClass (QName l)
  | EClassAsType (QName l)
  | ENotExported
      (Maybe (Name l)) -- optional parent, e.g. Bool in Bool(Right)
      (Name l)         -- the name which is not exported
      (ModuleName l)
  | EInternal String
  deriving (Data, Typeable, Show) -- FIXME write custom Show

instance (SrcInfo l) => SrcInfo (Scoped l) where
    toSrcInfo l1 ss l2 = None $ toSrcInfo l1 ss l2
    fromSrcInfo = None . fromSrcInfo
    getPointLoc = getPointLoc . sLoc
    fileName = fileName . sLoc
    startLine = startLine . sLoc
    startColumn = startColumn . sLoc
