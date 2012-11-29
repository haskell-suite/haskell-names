module Language.Haskell.Modules.GlobalSymbolTable (
  GST,
  symEmpty,
  symShow,
  SymFixity,
  sv_parent,
  symValueLookup,
  symValueAdd,
  symTypeLookup,
  symTypeAdd,
  ) where

import Language.Haskell.Exts.Annotated
import qualified Language.Haskell.Exts.Syntax as S
import Data.Monoid
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import Control.Exception

import Language.Haskell.Modules.SymbolInfo
import Language.Haskell.Modules.SyntaxUtils(nameToString, specialConToString)

type NameS = String
type ModuleNameS = String
-- | Possibly qualified name. If the name is not qualified,
-- 'ModuleNameS' is the empty string.
data GName = GName ModuleNameS NameS
  deriving (Eq, Ord, Show)
-- | Qualified name, where 'ModuleNameS' points to the module where the
-- name was originally defined. The module part is never empty.
type OrigName = GName

-- | Possibly ambiguous value symbol
type ASymValueInfo n = Either [SymValueInfo n] (SymValueInfo n)

-- | Possibly ambiguous type symbol
type ASymTypeInfo n = Either [SymTypeInfo n] (SymTypeInfo n)

-- | Global symbol table — contains global names
data GST = GST (Map.Map GName (ASymValueInfo OrigName)) (Map.Map GName (ASymTypeInfo OrigName))
    deriving (Show)

toGName :: QName l -> GName
toGName (UnQual _ n) = GName "" (nameToString n)
toGName (Qual _ (ModuleName _ m) n) = GName m (nameToString n)
toGName (Special _ s) = error "toGName: Special"

symEmpty :: GST
symEmpty = GST Map.empty Map.empty

symValueLookup :: QName l -> GST -> Maybe (ASymValueInfo GName)
symValueLookup qn (GST vs _) = Map.lookup (toGName qn) vs

symValueAdd :: QName l -> SymValueInfo OrigName -> GST -> GST
symValueAdd qn i (GST vs ts) = GST (Map.insertWith combineSyms (toGName qn) (Right i) vs) ts

symTypeLookup :: QName l -> GST -> Maybe (ASymTypeInfo GName)
symTypeLookup qn (GST _ ts) = Map.lookup (toGName qn) ts

symTypeAdd :: QName l -> SymTypeInfo OrigName -> GST -> GST
symTypeAdd qn i (GST vs ts) = GST vs (Map.insertWith combineSyms (toGName qn) (Right i) ts)

symShow :: GST -> String
symShow (GST vs ts) = unlines $ map (\ (i, _si) -> show i) (Map.toList vs)
                           ++ map (\ (i, _si) -> show i) (Map.toList ts)

combineSyms :: Eq i => Either [i] i -> Either [i] i -> Either [i] i
combineSyms (Right s1) (Right s2)
  | s1 == s2 = Right s1
  | otherwise = Left [s1, s2]
combineSyms (Left ss)  (Right s2) = Left $ s2 : ss
combineSyms (Right s1) (Left ss)  = Left $ s1 : ss
combineSyms (Left ss1) (Left ss2) = Left $ ss1 ++ ss2

combineType :: ASymTypeInfo n -> ASymTypeInfo n -> ASymTypeInfo n
combineType = undefined
