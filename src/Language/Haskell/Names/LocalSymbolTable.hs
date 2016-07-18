-- | This module is designed to be imported qualified.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.Names.LocalSymbolTable
  ( Table
  , empty
  , lookupValue
  , addValue
  ) where

import qualified Data.Map as Map
import Data.Monoid
import Language.Haskell.Exts
import Language.Haskell.Names.SyntaxUtils (dropAnn)
import Language.Haskell.Names.Types

-- | Local symbol table — contains locally bound names
newtype Table = Table (Map.Map (Name ()) SrcLoc)
  deriving Monoid

addValue :: SrcInfo l => Name l -> Table -> Table
addValue n (Table vs) =
  Table (Map.insert (dropAnn n) (getPointLoc $ ann n) vs)

lookupValue :: QName l -> Table -> Either (Error l) SrcLoc
lookupValue qn@(UnQual _ n) (Table vs) =
  maybe (Left $ ENotInScope qn) Right $
    Map.lookup (dropAnn n) vs
lookupValue qn _ = Left $ ENotInScope qn

empty :: Table
empty = Table Map.empty
