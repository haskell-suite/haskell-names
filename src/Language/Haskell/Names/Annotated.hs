-- This module uses the open recursion interface
-- ("Language.Haskell.Names.Open") to annotate the AST with binding
-- information.
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, ImplicitParams,
    UndecidableInstances, OverlappingInstances, ScopedTypeVariables,
    TypeOperators, GADTs #-}
module Language.Haskell.Names.Annotated
  ( Scoped(..)
  , NameInfo(..)
  , annotate
  ) where

import Language.Haskell.Names.Types
import Language.Haskell.Names.Open.Base
import Language.Haskell.Names.Open.Instances ()
import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import qualified Language.Haskell.Names.LocalSymbolTable as Local
import Language.Haskell.Exts.Annotated
import Data.Proxy
import Data.Lens.Common
import Data.Typeable
import Control.Applicative

-- This should be incorporated into Data.Typeable soon
import Type.Eq

annotate
  :: forall a l .
     (Resolvable (a (Scoped l)), Functor a, Typeable l)
  => Scope -> a l -> a (Scoped l)
annotate sc = annotateRec (Proxy :: Proxy l) sc . fmap (Scoped None)

annotateRec
  :: forall a l .
     (Typeable l, Resolvable a)
  => Proxy l -> Scope -> a -> a
annotateRec _ sc a = go sc a where
  go :: forall a . Resolvable a => Scope -> a -> a
  go sc a
    | Just (Eq :: QName (Scoped l) :~: a) <- dynamicEq
      = lookupValue (fmap sLoc a) sc <$ a
    | otherwise
      = rmap go sc a

lookupValue :: QName l -> Scope -> Scoped l
lookupValue qn sc = Scoped nameInfo (ann qn)
  where
    nameInfo =
      case Local.lookupValue qn $ getL lTable sc of
        Right r -> LocalValue r
        _ ->
          case Global.lookupValue qn $ getL gTable sc of
            Global.Result r -> GlobalValue r
            Global.Error e -> ScopeError e
            Global.Special -> None
