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
newtype Table = Table (Map.Map Name Ann.SrcLoc)
  deriving Monoid

addValue :: Ann.SrcInfo l => Ann.Name l -> Table -> Table
addValue n (Table vs) =
  Table (Map.insert (sName n) (Ann.getPointLoc $ Ann.ann n) vs)

lookupValue :: Ann.QName l -> Table -> Either (Error l) Ann.SrcLoc
lookupValue qn@(Ann.UnQual _ n) (Table vs) =
  maybe (Left $ ENotInScope qn) Right $
    Map.lookup (sName n) vs
lookupValue qn _ = Left $ ENotInScope qn

empty :: Table
empty = Table Map.empty
