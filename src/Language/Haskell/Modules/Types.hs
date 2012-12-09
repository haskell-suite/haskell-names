{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Modules.Types where

import Language.Haskell.Exts.Annotated
import Data.Typeable
import Data.Data
import Data.Monoid
import Control.Applicative
import Data.Lens.Common
import qualified Data.Set as Set
import {-# SOURCE #-} qualified Language.Haskell.Modules.GlobalSymbolTable as Global

type SymFixity = (Assoc (), Int)

data SymValueInfo name
    = SymValue       { sv_origName :: name, sv_fixity :: Maybe SymFixity }
    | SymMethod      { sv_origName :: name, sv_fixity :: Maybe SymFixity, sv_className :: name }
    | SymSelector    { sv_origName :: name, sv_fixity :: Maybe SymFixity, sv_typeName :: name }
    | SymConstructor { sv_origName :: name, sv_fixity :: Maybe SymFixity, sv_typeName :: name }
    deriving (Eq, Ord, Show, Data, Typeable)

data SymTypeInfo name
    = SymType        { st_origName :: name, st_fixity :: Maybe SymFixity }
    | SymData        { st_origName :: name, st_fixity :: Maybe SymFixity }
    | SymTypeFam     { st_origName :: name, st_fixity :: Maybe SymFixity }
    | SymDataFam     { st_origName :: name, st_fixity :: Maybe SymFixity }
    | SymClass       { st_origName :: name, st_fixity :: Maybe SymFixity }
    deriving (Eq, Ord, Show, Data, Typeable)

class HasOrigName i where
  origName :: i n -> n

instance HasOrigName SymValueInfo where
  origName = sv_origName

instance HasOrigName SymTypeInfo where
  origName = st_origName

data Symbols = Symbols (Set.Set (SymValueInfo OrigName)) (Set.Set (SymTypeInfo OrigName))
  deriving (Eq, Show, Data, Typeable)

instance Monoid Symbols where
  mempty = Symbols mempty mempty
  mappend (Symbols s1 t1) (Symbols s2 t2) =
    Symbols (s1 `mappend` s2) (t1 `mappend` t2)

valSyms :: Lens Symbols (Set.Set (SymValueInfo OrigName))
valSyms = lens (\(Symbols vs _) -> vs) (\vs (Symbols _ ts) -> Symbols vs ts)

tySyms :: Lens Symbols (Set.Set (SymTypeInfo OrigName))
tySyms = lens (\(Symbols _ ts) -> ts) (\ts (Symbols vs _) -> Symbols vs ts)

mkVal :: SymValueInfo OrigName -> Symbols
mkVal i = Symbols (Set.singleton i) mempty

mkTy :: SymTypeInfo OrigName -> Symbols
mkTy i = Symbols mempty (Set.singleton i)

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
    | ImportPart  { sLoc :: l, importSymbols :: Symbols }
    | Export      { sLoc :: l, exportSymbols :: Symbols }
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
  | EModNotFound (ModuleName l)
  | EExportConflict [(NameS, [ExportSpec l])]
  | EInternal String
  deriving (Data, Typeable, Show) -- FIXME write custom Show

instance (SrcInfo l) => SrcInfo (Scoped l) where
    toSrcInfo l1 ss l2 = None $ toSrcInfo l1 ss l2
    fromSrcInfo = None . fromSrcInfo
    getPointLoc = getPointLoc . sLoc
    fileName = fileName . sLoc
    startLine = startLine . sLoc
    startColumn = startColumn . sLoc
