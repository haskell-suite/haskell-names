-- | This module is designed to be imported qualified.
module Language.Haskell.Modules.GlobalSymbolTable
  ( Table
  , GName
  , OrigName
  , ASymValueInfo
  , ASymTypeInfo
  , empty
  , lookupValue
  , addValue
  , lookupType
  , addType
  ) where

import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Exts.Syntax as S
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Control.Exception

import Language.Haskell.Modules.Types
import Language.Haskell.Modules.SyntaxUtils(nameToString, specialConToString)

-- | Possibly ambiguous value symbol
type ASymValueInfo n = Either [SymValueInfo n] (SymValueInfo n)

-- | Possibly ambiguous type symbol
type ASymTypeInfo n = Either [SymTypeInfo n] (SymTypeInfo n)

-- | Global symbol table — contains global names
data Table = Table (Map.Map GName (ASymValueInfo OrigName)) (Map.Map GName (ASymTypeInfo OrigName))
    deriving (Show)

toGName :: QName l -> GName
toGName (UnQual _ n) = GName "" (nameToString n)
toGName (Qual _ (ModuleName _ m) n) = GName m (nameToString n)
toGName (Special _ s) = error "toGName: Special"

empty :: Table
empty = Table Map.empty Map.empty

lookupValue :: QName l -> Table -> Maybe (ASymValueInfo OrigName)
lookupValue qn (Table vs _) = Map.lookup (toGName qn) vs

addValue :: QName l -> SymValueInfo OrigName -> Table -> Table
addValue qn i (Table vs ts) = Table (Map.insertWith combineSyms (toGName qn) (Right i) vs) ts

lookupType :: QName l -> Table -> Maybe (ASymTypeInfo GName)
lookupType qn (Table _ ts) = Map.lookup (toGName qn) ts

addType :: QName l -> SymTypeInfo OrigName -> Table -> Table
addType qn i (Table vs ts) = Table vs (Map.insertWith combineSyms (toGName qn) (Right i) ts)

combineSyms :: Eq i => Either [i] i -> Either [i] i -> Either [i] i
combineSyms (Right s1) (Right s2)
  | s1 == s2 = Right s1
  | otherwise = Left [s1, s2]
combineSyms (Left ss)  (Right s2) = Left $ s2 : ss
combineSyms (Right s1) (Left ss)  = Left $ s1 : ss
combineSyms (Left ss1) (Left ss2) = Left $ ss1 ++ ss2

combineType :: ASymTypeInfo n -> ASymTypeInfo n -> ASymTypeInfo n
combineType = undefined
