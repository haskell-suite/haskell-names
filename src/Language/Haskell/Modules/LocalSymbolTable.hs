-- | This module is designed to be imported qualified.
module Language.Haskell.Modules.LocalSymbolTable
  ( Table
  , empty
  , lookupValue
  , addValue
  ) where

import qualified Data.Map as Map
import Language.Haskell.Exts.Annotated
import Language.Haskell.Modules.SyntaxUtils

-- | Local symbol table — contains locally bound names
newtype Table = Table (Map.Map String SrcLoc)

addValue :: SrcInfo l => Name l -> Table -> Table
addValue n (Table vs) =
  Table (Map.insert (nameToString n) (getPointLoc $ ann n) vs)

lookupValue :: QName l -> Table -> Maybe SrcLoc
lookupValue (UnQual _ n) (Table vs) =
  Map.lookup (nameToString n) vs
lookupValue _ _ = Nothing

empty :: Table
empty = Table Map.empty
