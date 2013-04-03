{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, UndecidableInstances, DefaultSignatures, OverlappingInstances, TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams #-}
module Language.Haskell.Modules.Open.Base where

import qualified Language.Haskell.Modules.GlobalSymbolTable as Global
import qualified Language.Haskell.Modules.LocalSymbolTable as Local
import Language.Haskell.Modules.SyntaxUtils
import Language.Haskell.Exts.Annotated
import Control.Applicative
import Data.List
import Data.Lens.Common
import Data.Lens.Template
import Data.Generics.Traversable
import Data.Proxy

data NameContext = Binding | Reference | Other

data Scope = Scope
  { _gTable :: Global.Table
  , _lTable :: Local.Table
  , _nameCtx :: NameContext
  }

makeLens ''Scope

newtype Alg w = Alg { runAlg :: forall d . Resolvable d => d -> Scope -> w d }

alg :: (?alg :: Alg w, Resolvable d) => d -> Scope -> w d
alg = runAlg ?alg

defaultRtraverse
  :: (GTraversable Resolvable a, Applicative f, ?alg :: Alg f)
  => a -> Scope -> f a
defaultRtraverse a sc =
  let ?c = Proxy :: Proxy Resolvable
  in gtraverse (\a -> alg a sc) a

class Resolvable a where
  rtraverse
    :: (Applicative f, ?alg :: Alg f)
    => a -> Scope -> f a

instance GTraversable Resolvable a => Resolvable a where
  rtraverse = defaultRtraverse

intro :: (SrcInfo l, GetBound a l) => a -> Scope -> Scope
intro node =
  modL lTable $
    \tbl -> foldl' (flip Local.addValue) tbl $ getBound node

setNameCtx :: NameContext -> Scope -> Scope
setNameCtx ctx = setL nameCtx ctx

binder :: Scope -> Scope
binder = setNameCtx Binding
