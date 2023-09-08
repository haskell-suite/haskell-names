module Language.Haskell.Names.ScopeUtils where

import Control.Arrow
import Language.Haskell.Names.Types
import Language.Haskell.Names.SyntaxUtils
import Language.Haskell.Exts
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import Control.Monad (guard)
import Data.List (nub)

scopeError :: Functor f => Error l -> f l -> f (Scoped l)
scopeError e f = Scoped (ScopeError e) <$> f

none :: l -> Scoped l
none = Scoped None

noScope :: (Annotated a) => a l -> a (Scoped l)
noScope = fmap none

symbolParent :: Symbol -> Maybe (Name ())
symbolParent (Selector { typeName = n }) = Just n
symbolParent (Constructor { typeName = n }) = Just n
symbolParent (Method { className = n }) = Just n
symbolParent (TypeFam { associate = as }) = as
symbolParent (DataFam { associate = as }) = as
symbolParent (PatternConstructor { patternTypeName = mn}) = mn
symbolParent (PatternSelector { patternTypeName = mn}) = mn
symbolParent _ = Nothing

computeSymbolTable
  :: Bool
    -- ^ If 'True' (\"qualified\"), then only the qualified names are
    -- inserted.
    --
    -- If 'False', then both qualified and unqualified names are insterted.
  -> ModuleName ()
  -> [Symbol]
  -> Global.Table
computeSymbolTable qual modulename symbols =
  Global.fromList (qualified <> if qual then [] else unqualified) where
    qualified = do
        symbol <- symbols
        return (Qual  ()modulename (symbolName symbol),symbol)
    unqualified = do
        symbol <- symbols
        return (UnQual () (symbolName symbol),symbol)

-- | Find a single constructor or method name in a list of symbols
resolveCName
  :: [Symbol]
  -> Name ()
  -> (CName l -> Error l) -- ^ error for "not found" condition
  -> CName l
  -> (CName (Scoped l), [Symbol])
resolveCName symbols parent notFound cn =
  let
    vs = nub (do
        symbol <- symbols
        guard (Global.isValue symbol)
        let name = symbolName symbol
        guard (dropAnn (unCName cn) == name)
        Just p <- return $ symbolParent symbol
        guard (p == parent)
        return symbol)
  in
    case vs of
      [] -> (scopeError (notFound cn) cn, [])
      [symbol] -> (Scoped (GlobalSymbol symbol (UnQual () (dropAnn (unCName cn)))) <$> cn, [symbol])
      _ -> (scopeError (EInternal "resolveCName") cn, [])

-- | Find a list of constructor or method names in a list of symbols.
resolveCNames
  :: [Symbol]
  -> Name ()
  -> (CName l -> Error l) -- ^ error for "not found" condition
  -> [CName l]
  -> ([CName (Scoped l)], [Symbol])
resolveCNames syms orig notFound =
  second mconcat . unzip . map (resolveCName syms orig notFound)
