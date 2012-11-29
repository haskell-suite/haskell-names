{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.Modules.Types where

import Language.Haskell.Exts.Annotated hiding (fixities)
import Language.Haskell.Modules.Error
import Data.Typeable
import Data.Data

type NameS = String
type ModuleNameS = String

data Scoped l
    = Global     { sLoc :: l, sOrginalName :: QName SrcLoc }
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
