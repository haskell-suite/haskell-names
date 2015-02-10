{-# LANGUAGE DeriveDataTypeable #-}
-- | This module is designed to be imported qualified.
module Language.Haskell.Names.GlobalSymbolTable where

import Language.Haskell.Exts (
    QName)
import qualified Language.Haskell.Exts.Annotated as Ann (
    QName)
import Language.Haskell.Exts.Annotated.Simplify (
    sQName)

import Data.Map (
    Map)
import qualified Data.Map as Map (
    empty,unionWith,fromListWith,lookup,map)

import Control.Arrow
import Data.List as List (union)

import Language.Haskell.Names.Types

-- | Global symbol table — contains names declared somewhere at the top level.
type Table = Map QName [Symbol]

-- | Empty global symbol table.
empty :: Table
empty = Map.empty

-- | For each name take the union of the lists of symbols they refer to.
mergeTables :: Table -> Table -> Table
mergeTables = Map.unionWith List.union

data Result l
  = SymbolFound Symbol
  | Error (Error l)
  | Special

lookupValue :: Ann.QName l -> Table -> Result l
lookupValue qn = lookupName qn . filterTable isValue

lookupType :: Ann.QName l -> Table -> Result l
lookupType qn = lookupName qn . filterTable isType

lookupMethodOrAssociate :: Ann.QName l -> Table -> Result l
lookupMethodOrAssociate qn = lookupName qn . filterTable isMethodOrAssociated

lookupName :: Ann.QName l -> Table -> Result l
lookupName qn table = case Map.lookup (sQName qn) table of
    Nothing -> Error $ ENotInScope qn
    Just [] -> Error $ ENotInScope qn
    Just [i] -> SymbolFound i
    Just is -> Error $ EAmbiguous qn is

filterTable :: (Symbol -> Bool) -> Table -> Table
filterTable relevant = Map.map (filter relevant)

isValue :: Symbol -> Bool
isValue symbol = case symbol of
    Value {} -> True
    Method {} -> True
    Selector {} -> True
    Constructor {} -> True
    _ -> False

isType :: Symbol -> Bool
isType symbol = case symbol of
    Type {} -> True
    Data {} -> True
    NewType {} -> True
    TypeFam {} -> True
    DataFam {} -> True
    Class   {} -> True
    _ -> False

isMethodOrAssociated :: Symbol -> Bool
isMethodOrAssociated symbol = case symbol of
    Method {} -> True
    TypeFam {} -> True
    DataFam {} -> True
    _ -> False

fromList :: [(QName,Symbol)] -> Table
fromList = Map.fromListWith List.union . map (second (:[]))

