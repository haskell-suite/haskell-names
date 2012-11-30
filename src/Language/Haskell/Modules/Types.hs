{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Modules.Types where

import Language.Haskell.Exts.Annotated hiding (fixities)
import Language.Haskell.Modules.Error
import Data.Typeable
import Data.Data

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
    = Global     { sLoc :: l, sOrginalName :: OrigName }
    | Local      { sLoc :: l, sDefLoc :: SrcLoc }
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
