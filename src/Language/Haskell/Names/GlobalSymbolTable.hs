{-# LANGUAGE DeriveDataTypeable #-}
-- | This module is designed to be imported qualified.
module Language.Haskell.Names.GlobalSymbolTable where

import Language.Haskell.Exts (
    QName(Qual,UnQual,Special),ModuleName(ModuleName))
import qualified Language.Haskell.Exts.Annotated as Ann (
    QName(Qual,UnQual),Name,ann,ModuleName(ModuleName))
import Language.Haskell.Exts.Annotated.Simplify (
    sQName,sName)

import Language.Haskell.Names.SyntaxUtils (setAnn,annName)

import Data.Map (
    Map)
import qualified Data.Map as Map (
    empty,unionWith,fromListWith,lookup,map,fromList,toList)

import Control.Arrow
import Data.List as List (union)
import Control.Monad (guard)

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

-- | Methods can sometimes be referenced unqualified and still be resolved to
--   a symbols that is only in scope qualified.
--   https://www.haskell.org/pipermail/haskell-prime/2008-April/002569.html
--   The test for this is tests/annotations/QualifiedMethods.hs
lookupMethod :: Ann.Name l -> Table -> (Result l,Maybe QName)
lookupMethod name tbl = (case Map.lookup unqualifiedName qualificationTable of
        Nothing -> (Error (ENotInScope (Ann.UnQual (Ann.ann name) name)),Nothing)
        Just qn -> (lookupName qn (filterTable isMethod tbl),Just (sQName qn))) where
    unqualifiedName = UnQual (sName name)
    qualificationTable = Map.fromList (do
        (qn,symbols) <- Map.toList tbl
        guard (any isMethod symbols)
        case qn of
            Qual (ModuleName m) n -> return (UnQual n,Ann.Qual (Ann.ann name) (Ann.ModuleName (Ann.ann name) m) (setAnn (Ann.ann name) (annName n)))
            UnQual n -> return (UnQual n,Ann.UnQual (Ann.ann name) (setAnn (Ann.ann name) (annName n)))
            Language.Haskell.Exts.Special _ -> [])

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

isMethod :: Symbol -> Bool
isMethod symbol = case symbol of
    Method {} -> True
    _ -> False

fromList :: [(QName,Symbol)] -> Table
fromList = Map.fromListWith List.union . map (second (:[]))

