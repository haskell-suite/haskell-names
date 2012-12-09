{-# LANGUAGE DeriveDataTypeable #-}
-- | This module is designed to be imported qualified.
module Language.Haskell.Modules.GlobalSymbolTable
  ( Table
  , GName
  , OrigName
  , empty
  , lookupValue
  , addValue
  , lookupType
  , addType
  , fromLists
  ) where

import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Exts.Syntax as S
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Data.Typeable
import Data.Data
import Control.Exception
import Control.Arrow

import Language.Haskell.Modules.Types
import Language.Haskell.Modules.SyntaxUtils(nameToString, specialConToString)

-- | Possibly ambiguous value symbol
type ASymValueInfo n = Either [SymValueInfo n] (SymValueInfo n)

-- | Possibly ambiguous type symbol
type ASymTypeInfo n = Either [SymTypeInfo n] (SymTypeInfo n)

-- | Global symbol table — contains global names
data Table = Table (Map.Map GName (ASymValueInfo OrigName)) (Map.Map GName (ASymTypeInfo OrigName))
    deriving (Eq, Show, Data, Typeable)

instance Monoid Table where
  mempty = empty
  mappend (Table vs1 ts1) (Table vs2 ts2) =
    Table (j vs1 vs2) (j ts1 ts2)
    where
      j :: (Eq i, Ord k)
        => Map.Map k (Either [i] i)
        -> Map.Map k (Either [i] i)
        -> Map.Map k (Either [i] i)
      j = Map.unionWith combineSyms

toGName :: QName l -> GName
toGName (UnQual _ n) = GName "" (nameToString n)
toGName (Qual _ (ModuleName _ m) n) = GName m (nameToString n)
toGName (Special _ s) = error "toGName: Special"

empty :: Table
empty = Table Map.empty Map.empty

lookupValue
  :: QName l
  -> Table
  -> Either (Error l) (SymValueInfo OrigName)
lookupValue qn (Table vs _) =
  case Map.lookup (toGName qn) vs of
    Nothing -> Left $ ENotInScope qn
    Just (Left ts) ->
      Left $ EAmbiguous qn (map sv_origName ts)
    Just (Right i) -> Right i

addValue :: QName l -> SymValueInfo OrigName -> Table -> Table
addValue qn i (Table vs ts) = Table (Map.insertWith combineSyms (toGName qn) (Right i) vs) ts

lookupType
  :: QName l
  -> Table
  -> Either (Error l) (SymTypeInfo OrigName)
lookupType qn (Table _ ts) =
  case Map.lookup (toGName qn) ts of
    Nothing -> Left $ ENotInScope qn
    Just (Left ts) ->
      Left $ EAmbiguous qn (map st_origName ts)
    Just (Right i) -> Right i

addType :: QName l -> SymTypeInfo OrigName -> Table -> Table
addType qn i (Table vs ts) = Table vs (Map.insertWith combineSyms (toGName qn) (Right i) ts)

fromLists
  :: ([(GName, SymValueInfo OrigName)],
      [(GName, SymTypeInfo OrigName)])
  -> Table
fromLists (vs, ts) =
  Table
    (Map.fromListWith combineSyms $ map (second Right) vs)
    (Map.fromListWith combineSyms $ map (second Right) ts)

combineSyms :: Eq i => Either [i] i -> Either [i] i -> Either [i] i
combineSyms (Right s1) (Right s2)
  | s1 == s2 = Right s1
  | otherwise = Left [s1, s2]
combineSyms (Left ss)  (Right s2) = Left $ s2 : ss
combineSyms (Right s1) (Left ss)  = Left $ s1 : ss
combineSyms (Left ss1) (Left ss2) = Left $ ss1 ++ ss2
