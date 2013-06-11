{-# LANGUAGE RankNTypes, FlexibleInstances, FlexibleContexts, UndecidableInstances, DefaultSignatures, OverlappingInstances, TemplateHaskell, ScopedTypeVariables #-}
{-# LANGUAGE ImplicitParams, KindSignatures #-}
module Language.Haskell.Names.Open.Base where

import qualified Language.Haskell.Names.GlobalSymbolTable as Global
import qualified Language.Haskell.Names.LocalSymbolTable as Local
import Language.Haskell.Names.SyntaxUtils
import Language.Haskell.Exts.Annotated
import Control.Applicative
import Control.Monad.Identity
import Data.List
import Data.Lens.Common
import Data.Lens.Template
import Data.Generics.Traversable
import Data.Typeable
import GHC.Exts (Constraint)

data NameContext = Binding | Reference | Other

data Scope = Scope
  { _gTable :: Global.Table
  , _lTable :: Local.Table
  , _nameCtx :: NameContext
  }

makeLens ''Scope

initialScope :: Scope
initialScope = Scope Global.empty Local.empty Reference

newtype Alg w = Alg
  { runAlg :: forall d . Resolvable d => d -> Scope -> w d }

alg :: (?alg :: Alg w, Resolvable d) => d -> Scope -> w d
alg = runAlg ?alg

data ConstraintProxy (p :: * -> Constraint) = ConstraintProxy

defaultRtraverse
  :: (GTraversable Resolvable a, Applicative f, ?alg :: Alg f)
  => a -> Scope -> f a
defaultRtraverse a sc =
  let ?c = ConstraintProxy :: ConstraintProxy Resolvable
  in gtraverse (\a -> alg a sc) a

-- We use Typeable here rather than a class-based approach.
-- Otherwise, hand-written instances would carry extremely long lists of
-- constraints, saying that the subterms satisfy the user-supplied class.
class Typeable a => Resolvable a where
  rtraverse
    :: (Applicative f, ?alg :: Alg f)
    => a -> Scope -> f a

instance (Typeable a, GTraversable Resolvable a) => Resolvable a where
  rtraverse = defaultRtraverse

-- analogous to gmap, but for Resolvable
rmap
  :: Resolvable a
  => (forall b. Resolvable b => Scope -> b -> b)
  -> Scope -> a -> a
rmap f sc =
  let ?alg = Alg $ \a sc -> Identity (f sc a)
  in runIdentity . flip rtraverse sc

intro :: (SrcInfo l, GetBound a l) => a -> Scope -> Scope
intro node =
  modL lTable $
    \tbl -> foldl' (flip Local.addValue) tbl $ getBound node

setNameCtx :: NameContext -> Scope -> Scope
setNameCtx ctx = setL nameCtx ctx

binder :: Scope -> Scope
binder = setNameCtx Binding
