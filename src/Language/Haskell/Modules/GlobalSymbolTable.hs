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
  , types
  , values
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
import Control.Applicative hiding (empty)
import Data.Lens.Common

import Language.Haskell.Modules.Types
import Language.Haskell.Modules.SyntaxUtils(nameToString, specialConToString)

-- | Global symbol table — contains global names
data Table =
  Table
    (Map.Map GName (Set.Set (SymValueInfo OrigName)))
    (Map.Map GName (Set.Set (SymTypeInfo  OrigName)))
    deriving (Eq, Show, Data, Typeable)

valLens :: Lens Table (Map.Map GName (Set.Set (SymValueInfo OrigName)))
valLens = lens (\(Table vs _) -> vs) (\vs (Table _ ts) -> Table vs ts)

tyLens :: Lens Table (Map.Map GName (Set.Set (SymTypeInfo OrigName)))
tyLens = lens (\(Table _ ts) -> ts) (\ts (Table vs _) -> Table vs ts)

instance Monoid Table where
  mempty = empty
  mappend (Table vs1 ts1) (Table vs2 ts2) =
    Table (j vs1 vs2) (j ts1 ts2)
    where
      j :: (Ord i, Ord k)
        => Map.Map k (Set.Set i)
        -> Map.Map k (Set.Set i)
        -> Map.Map k (Set.Set i)
      j = Map.unionWith Set.union

toGName :: QName l -> GName
toGName (UnQual _ n) = GName "" (nameToString n)
toGName (Qual _ (ModuleName _ m) n) = GName m (nameToString n)
toGName (Special _ s) = error "toGName: Special"

empty :: Table
empty = Table Map.empty Map.empty

lookupL
  :: HasOrigName i
  => Lens Table (Map.Map GName (Set.Set (i OrigName)))
  -> QName l
  -> Table
  -> Either (Error l) (i OrigName)
lookupL lens qn tbl =
  case Set.toList <$> (Map.lookup (toGName qn) $ getL lens tbl) of
    Nothing -> Left $ ENotInScope qn
    Just [] -> Left $ ENotInScope qn
    Just [i] -> Right i
    Just is -> Left $ EAmbiguous qn (map origName is)

lookupValue :: QName l -> Table -> Either (Error l) (SymValueInfo OrigName)
lookupValue = lookupL valLens

lookupType :: QName l -> Table -> Either (Error l) (SymTypeInfo OrigName)
lookupType  = lookupL tyLens

addL
  :: (HasOrigName i, Ord (i OrigName))
  => Lens Table (Map.Map GName (Set.Set (i OrigName)))
  -> QName l
  -> i OrigName
  -> Table -> Table
addL lens qn i = modL lens (Map.insertWith Set.union (toGName qn) (Set.singleton i))

addValue :: QName l -> SymValueInfo OrigName -> Table -> Table
addValue = addL valLens

addType :: QName l -> SymTypeInfo OrigName -> Table -> Table
addType = addL tyLens

fromLists
  :: ([(GName, SymValueInfo OrigName)],
      [(GName, SymTypeInfo OrigName)])
  -> Table
fromLists (vs, ts) =
  Table
    (Map.fromListWith Set.union $ map (second Set.singleton) vs)
    (Map.fromListWith Set.union $ map (second Set.singleton) ts)

values :: Table -> Map.Map GName (Set.Set (SymValueInfo OrigName))
types  :: Table -> Map.Map GName (Set.Set (SymTypeInfo  OrigName))
values = getL valLens
types = getL tyLens
